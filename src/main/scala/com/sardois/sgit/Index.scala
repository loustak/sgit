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
        if (!map.contains(relativePath)) {
            throw new RuntimeException("File not in the index: " + relativePath)
        }

        Index(map - relativePath)
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

    def modified(otherIndex: Index): List[String] = {
        otherIndex.map.keys.filter( key => {
            if (map.contains(key)) {
                // Return true if sha changed for the same file
                otherIndex.map(key) != map(key)
            } else false
        }).toList
    }

    def deleted(otherIndex: Index): List[String] = {
        map.keys.filter( key => {
            !otherIndex.map.contains(key)
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
    def getStatusNotStagedModifiedFiles(repoFolder: File, index: Index, paths: List[String]): List[String] = {
        val indexEntries = IOIndexEntry.fromPaths(repoFolder, paths)
        val tmpIndex = Index(indexEntries)
        index.modified(tmpIndex)
    }

    @impure
    def getStatusNotStagedDeletedFiles(index: Index, paths: List[String]): List[String] = {
        val indexEntries = IndexEntry.fromPathsWithEmptySha(paths)
        val tmpIndex = Index(indexEntries)
        index.deleted(tmpIndex)
    }

    @impure
    def getStatusStagedModifiedFiles(newIndex: Index, oldIndex: Index): List[String] = {
        oldIndex.modified(newIndex)
    }

    @impure
    def getStatusStagedDeletedFiles(newIndex: Index, oldIndex: Index): List[String] = {
        oldIndex.deleted(newIndex)
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Unit = {
        val blobsFolder = IOBlob.getBlobsFolder(repoFolder)
        val indexFile = getIndexFile(repoFolder)
        val index = read(indexFile)

        // Split files in two, the one to try to add
        // and the one that must be deleted
        val files = Util.pathsToFiles(args.paths)
        val (filesToAdd, filesToRemove) = files.partition( file => {
            file.exists
        })

        val filesToAddCleaned = Util.removeDirectories(Util.getNestedFiles(filesToAdd))
        val filesToRemoveCleaned = Repository.relativizesFile(repoFolder, filesToRemove)

        val indexEntriesToAdd = IOIndexEntry.fromFiles(repoFolder, filesToAddCleaned)
        val indexEntriesToRemove = IndexEntry.fromPathsWithEmptySha(filesToRemoveCleaned)
        val newIndex = index.addAll(indexEntriesToAdd).removeAll(indexEntriesToRemove)

        write(indexFile, newIndex)
        IOBlob.writeAll(blobsFolder, filesToAddCleaned)
    }

    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Unit = {
        // TODO: Check that the file has been committed
        val relativePaths = args.paths.map( path => {
            Repository.relativize(repoFolder, File(path))
        })
        val indexEntriesToRemove = IndexEntry.fromPathsWithEmptySha(relativePaths)
        val indexFile = getIndexFile(repoFolder)
        val index = read(indexFile)

        val newIndex = index.removeAll(indexEntriesToRemove)

        write(indexFile, newIndex)
    }
}
