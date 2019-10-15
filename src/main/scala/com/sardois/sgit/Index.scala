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

    def addAll(indexEntries: Iterable[IndexEntry]): Index = {

        @tailrec
        def rec(indexEntries: Iterable[IndexEntry], index: Index): Index = {
            if (indexEntries.isEmpty) index
            else {
                val head = indexEntries.head
                val tail = indexEntries.tail
                rec(tail, index.add(head))
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

    def removeAll(indexEntries: Iterable[IndexEntry]): Index = {

        @tailrec
        def rec(indexEntries: Iterable[IndexEntry], index: Index): Index = {
            if (indexEntries.isEmpty) index
            else {
                val head = indexEntries.head
                val tail = indexEntries.tail
                rec(tail, index.remove(head))
            }
        }

        rec(indexEntries, this)
    }

    def untracked(otherIndex: Index): Iterable[String] = {
        otherIndex.map.keys.filter( key => {
            !map.contains(key)
        })
    }

    def newfiles(otherIndex: Index): Iterable[String] = {
        map.keys.filter( key => {
            !otherIndex.map.contains(key)
        })
    }

    def modified(otherIndex: Index): Iterable[String] = {
        otherIndex.map.keys.filter( key => {
            if (map.contains(key)) {
                // Return true if sha changed for the same file
                otherIndex.map(key) != map(key)
            } else false
        })
    }

    def deleted(otherIndex: Index): Iterable[String] = {
        otherIndex.map.keys.filter( key => {
            !map.contains(key)
        })
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

    def apply(indexEntries: Iterable[IndexEntry]): Index = {
        Index().addAll(indexEntries)
    }
}

object IOIndex {

    def getIndexesFolder(repoFolder: File): File = {
        repoFolder/Repository.indexesPath
    }

    def getIndexFile(repoFolder: File): File = {
        repoFolder/Repository.indexPath
    }

    def read(indexFile: File): Index = {
        if (!indexFile.exists) {
            throw new RuntimeException("Index doesn't exists at " + indexFile.pathAsString)
        }

        val lines = indexFile.lines().toList

        @tailrec
        def rec(lines: Iterable[String], index: Index): Index = {
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
    def getUntrackedFiles(repoFolder: File, index: Index, paths: Iterable[String]): Iterable[String] = {
        val indexEntries = IOIndexEntry.fromPaths(repoFolder, paths)
        val tmpIndex = Index(indexEntries)
        index.untracked(tmpIndex)
    }

    @impure
    def getNotStagedModifiedFiles(repoFolder: File, index: Index, paths: Iterable[String]): Iterable[String] = {
        val indexEntries = IOIndexEntry.fromPaths(repoFolder, paths)
        val tmpIndex = Index(indexEntries)
        index.modified(tmpIndex)
    }

    @impure
    def getNotStagedDeletedFiles(repoFolder: File, index: Index, paths: Iterable[String]): Iterable[String] = {
        val files = Util.pathsToFiles(paths)
        val realFiles = files.filter( file => file.exists)
        val indexEntries = IOIndexEntry.fromFiles(repoFolder, realFiles)
        val newIndex = Index(indexEntries)
        newIndex.deleted(index)
    }

    def getStagedNewFiles(newIndex: Index, oldIndex: Index): Iterable[String] = {
        newIndex.newfiles(oldIndex)
    }

    def getStagedModifiedFiles(newIndex: Index, oldIndex: Index): Iterable[String] = {
        newIndex.modified(oldIndex)
    }

    def getStagedDeletedFiles(newIndex: Index, oldIndex: Index): Iterable[String] = {
        newIndex.deleted(oldIndex)
    }

    @impure
    def haveNotStagedChanges(repoFolder: File, index: Index, paths: Iterable[String]): Boolean = {
        getNotStagedModifiedFiles(repoFolder, index, paths).nonEmpty ||
        getNotStagedDeletedFiles(repoFolder, index, paths).nonEmpty
    }

    @impure
    def throwIfNotStagedChanges(repoFolder: File, index: Index, paths: Iterable[String]): Unit = {
        if (haveNotStagedChanges(repoFolder, index, paths)) {
            throw new RuntimeException("You have unstaged changes, please first commit your changes")
        }
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

        // Get the list of files
        val stagedNewFiles = getStagedNewFiles(index, oldIndex)
        val stagedModifiedFiles = getStagedModifiedFiles(index, oldIndex)
        val stagedDeletedFiles = getStagedDeletedFiles(index, oldIndex)
        val notStagedModifiedFiles = getNotStagedModifiedFiles(repoFolder, index, paths)
        val notStagedDeletedFiles = getNotStagedDeletedFiles(repoFolder, index, paths)
        val untrackedFiles = getUntrackedFiles(repoFolder, index, paths)

        // Transform the list of files to strings
        val stagedNewFilesString = Util.formatList(stagedNewFiles, "added ")
        val stagedModifiedFilesString = Util.formatList(stagedModifiedFiles, "modified ")
        val stagedDeletedFilesString = Util.formatList(stagedDeletedFiles, "deleted ")
        val notStagedModifiedFilesString = Util.formatList(notStagedModifiedFiles, "modified ")
        val notStagedDeletedFilesString = Util.formatList(notStagedDeletedFiles, "deleted ")
        val untrackedString = Util.formatList(untrackedFiles, "untracked ")

        val stringList = List(
            "Staged files:", stagedNewFilesString, stagedModifiedFilesString, stagedDeletedFilesString,
            "Not staged files:", notStagedModifiedFilesString, notStagedDeletedFilesString,
            "Not tracked files:", untrackedString
        )

        val newLine = System.lineSeparator()
        Some(stringList.mkString(newLine))
    }
}
