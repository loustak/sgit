package com.sardois.sgit

import better.files.File

import scala.annotation.tailrec

class Index(private val map: Map[String, String]) {

    def size: Int = map.size

    def add(path: String, sha: String): Index = {
        Index(map + (path -> sha))
    }

    def add(indexEntry: IndexEntry): Index = {
        val path = indexEntry.relativePath
        val sha = indexEntry.sha
        add(path, sha)
    }

    def addAll(indexEntries: List[IndexEntry]): Index = {

        @tailrec
        def rec(indexEntries: List[IndexEntry], index: Index): Index = {
            indexEntries match {
                case ::(head, next) => rec(next, index.add(head))
                case Nil => index
            }
        }

        rec(indexEntries, this)
    }

    def remove(relativePath: String): Index = {
        val newMap = map.filter( tuple => {
            val key = tuple._1
            !key.contains(relativePath)
        })

        Index(newMap)
    }

    def remove(indexEntry: IndexEntry): Index = {
        remove(indexEntry.relativePath)
    }

    def removeAll(indexEntries: List[IndexEntry]): Index = {

        @tailrec
        def rec(indexEntries: List[IndexEntry], index: Index): Index = {
            indexEntries match {
                case ::(head, next) => rec(next, index.remove(head))
                case Nil => index
            }
        }

        rec(indexEntries, this)
    }

    def untracked(otherIndex: Index): List[String] = {
        otherIndex.map.keys.filter( key => {
            !map.contains(key)
        }).toList
    }

    def newfiles(otherIndex: Index): List[String] = {
        map.keys.filter( key => {
            !otherIndex.map.contains(key)
        }).toList
    }

    def modified(otherIndex: Index): List[String] = {
        otherIndex.map.keys.filter( key => {
            if (map.contains(key)) {
                // Return true if sha changed for the same file
                otherIndex.map(key) != map(key)
            } else false
        }).toList
    }

    def deleted(otherIndex: Index): List[String] = {
        otherIndex.map.keys.filter( key => {
            !map.contains(key)
        }).toList
    }

    def sha(): String = {
        Util.shaString(toString)
    }

    override def toString: String = {
        map.keys.map( path => {
            path + " " + map(path)
        }).mkString(System.lineSeparator())
    }
}

object Index {

    def apply(map: Map[String, String] = Map()): Index = {
        new Index(map)
    }

    def apply(indexEntries: List[IndexEntry]): Index = {
        Index().addAll(indexEntries)
    }
}

object IOIndex {

    def getIndexesFolder(repoFolder: File): File = {
        repoFolder/Repository.getIndexesPath()
    }

    def getIndexFile(repoFolder: File): File = {
        repoFolder/Repository.getIndexPath()
    }

    def read(indexFile: File): Index = {
        if (!indexFile.exists) {
            throw new RuntimeException("Index doesn't exists at " + indexFile.pathAsString)
        }

        val lines = indexFile.lines().toList

        @tailrec
        def rec(lines: List[String], index: Index): Index = {
            if (lines == Nil) return index
            val split = lines.head.split(" ")
            if (split.length < 2) {
                throw new IllegalArgumentException("An entry in the index file is invalid")
            }

            val path = split(0)
            val sha = split(1)

            val newIndex = index.add(path, sha)
            rec(lines.tail, newIndex)
        }

        rec(lines, Index())
    }

    @impure
    def read(indexFolder: File, indexSha: String): Index = {
        val indexFile = indexFolder/indexSha
        read(indexFile)
    }

    @impure
    def getIndex(repoFolder: File): Index = {
        val indexFile = getIndexFile(repoFolder)
        read(indexFile)
    }

    @impure
    def write(indexFile: File, index: Index): Unit = {
        indexFile.clear()
        indexFile.write(index.toString)
    }

    @impure
    def getUntrackedFiles(repoFolder: File, index: Index, paths: List[String]): List[String] = {
        val indexEntries = IOIndexEntry.fromPaths(repoFolder, paths)
        val tmpIndex = Index(indexEntries)
        index.untracked(tmpIndex)
    }

    @impure
    def getNotStagedModifiedFiles(repoFolder: File, index: Index, paths: List[String]): List[String] = {
        val indexEntries = IOIndexEntry.fromPaths(repoFolder, paths)
        val tmpIndex = Index(indexEntries)
        index.modified(tmpIndex)
    }

    @impure
    def getNotStagedDeletedFiles(index: Index, paths: List[String]): List[String] = {
        val indexEntries = IndexEntry.fromPathsWithEmptySha(paths)
        val tmpIndex = Index(indexEntries)
        index.deleted(tmpIndex)
    }

    @impure
    def getStagedNewFiles(newIndex: Index, oldIndex: Index): List[String] = {
        newIndex.newfiles(oldIndex)
    }

    @impure
    def getStagedModifiedFiles(newIndex: Index, oldIndex: Index): List[String] = {
        newIndex.modified(oldIndex)
    }

    @impure
    def getStagedDeletedFiles(newIndex: Index, oldIndex: Index): List[String] = {
        newIndex.deleted(oldIndex)
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val blobsFolder = IOBlob.getBlobsFolder(repoFolder)
        val indexFile = getIndexFile(repoFolder)
        val index = read(indexFile)

        // Split files in two, the one to try to add
        // and the one that must be deleted
        val files = Util.pathsToFiles(args.paths)
        val filesToAdd  = files.filter( file => file.exists)

        val filesToAddCleaned = Util.removeDirectories(Util.getNestedFiles(filesToAdd))
        val filesToRemoveCleaned = Repository.relativizesFile(repoFolder, files)

        val indexEntriesToAdd = IOIndexEntry.fromFiles(repoFolder, filesToAddCleaned)
        val indexEntriesToRemove = IndexEntry.fromPathsWithEmptySha(filesToRemoveCleaned)
        val newIndex = index.removeAll(indexEntriesToRemove).addAll(indexEntriesToAdd)

        write(indexFile, newIndex)
        IOBlob.writeAll(blobsFolder, filesToAddCleaned)

        None
    }

    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        // TODO: Check that the file has been committed, it this function needed?
        val relativePaths = args.paths.map( path => {
            Repository.relativize(repoFolder, File(path))
        })
        val indexEntriesToRemove = IndexEntry.fromPathsWithEmptySha(relativePaths)
        val indexFile = getIndexFile(repoFolder)
        val index = read(indexFile)

        val newIndex = index.removeAll(indexEntriesToRemove)

        write(indexFile, newIndex)

        None
    }

    @impure
    def status(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val index = IOIndex.getIndex(repoFolder)
        val oldIndex = IOHead.getPointedIndex(repoFolder)

        val files = Util.removeDirectories(repoFolder.parent.listRecursively.toList)
        val paths = Util.filesToPath(files)

        val untrackedFiles = getUntrackedFiles(repoFolder, index, paths)
        val notStagedModifiedFiles = getNotStagedModifiedFiles(repoFolder, index, paths)
        val notStagedDeletedFiles = getNotStagedDeletedFiles(index, paths)
        val stagedNewFiles = getStagedNewFiles(index, oldIndex)
        val stagedModifiedFiles = getStagedModifiedFiles(index, oldIndex)
        val stagedDeletedFiles = getStagedDeletedFiles(index, oldIndex)

        val newLine = System.lineSeparator()

        val stagedNewFilesString = Util.formatList(stagedNewFiles, "added ")
        val stagedModifiedFilesString = Util.formatList(stagedModifiedFiles, "modified ")
        val stagedDeletedFilesString = Util.formatList(stagedDeletedFiles, "removed ")

        val notStagedModifiedFilesString = Util.formatList(notStagedModifiedFiles, "modified ")
        val notStagedDeletedFilesString = Util.formatList(notStagedDeletedFiles, "removed ")

        val untrackedString = Util.formatList(untrackedFiles, "untracked ")

        val stringList = List(
            "Staged files:", stagedNewFilesString, stagedModifiedFilesString, stagedDeletedFilesString,
            "Not staged files:", notStagedModifiedFilesString, notStagedDeletedFilesString,
            "Not tracked files:", untrackedString
        )

        Some(stringList.mkString(newLine))
    }
}
