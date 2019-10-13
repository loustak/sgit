package com.sardois.sgit


import better.files.File

import scala.annotation.tailrec

class IndexEntry(val relativePath: String, val sha: String) {

}

object IndexEntry {

    def apply(relativePath: String, sha: String): IndexEntry = new IndexEntry(relativePath, sha)

    def fromFile(repoFolder: File, file: File): IndexEntry = {
        val relativePath = Repository.relativePathFromRepo(repoFolder, file)
        val sha = Util.shaFile(file)
        IndexEntry(relativePath, sha)
    }
}

object IndexEntries {

    def apply(files: List[(String, String)]): List[IndexEntry] = {
        files.map( file => IndexEntry(file._1, file._2))
    }

    def fromFiles(repoFolder: File, files: List[File]): List[IndexEntry] = {
        files.map( file => IndexEntry.fromFile(repoFolder, file))
    }
}

class Index(map: Map[String, String]) {

    def size: Int = map.size

    def isTracked(indexEntry: IndexEntry): Boolean = {
        map.contains(indexEntry.relativePath)
    }

    def isModified(indexEntry: IndexEntry): Boolean = {
        if (!isTracked(indexEntry)) {
            throw new IllegalArgumentException("The file " +
                indexEntry.relativePath + " is not tracked by sgit")
        }

        val indexedSha = map(indexEntry.relativePath)
        indexEntry.sha != indexedSha
    }

    def addLine(path: String, sha: String): Index = {
        Index(map + (path -> sha))
    }

    def add(file: IndexEntry): Index = {
        val path = file.relativePath
        val sha = file.sha
        addLine(path, sha)
    }

    def addAll(stagedFiles: List[IndexEntry]): Index = {

        @tailrec
        def rec(stagedFiles: List[IndexEntry], index: Index): Index = {
            stagedFiles match {
                case ::(head, next) => rec(next, index.add(head))
                case Nil => index
            }
        }

        rec(stagedFiles, this)
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

    def apply(map: Map[String, String] = Map()): Index = new Index(map)
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

            val newIndex = index.addLine(path, sha)
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
    def write(indexFile: File, index: Index): Unit = {
        indexFile.clear()
        indexFile.write(index.toString)
    }

    @impure
    def getUntrackedFiles(repoFolder: File, index: Index, files: List[File]): List[File] = {
        files.filter( file => {
            if (file.isDirectory) false
            else {
                val indexEntry = IndexEntry.fromFile(repoFolder, file)
                !index.isTracked(indexEntry)
            }
        })
    }

    @impure
    def getModifiedFiles(repoFolder: File, index: Index, files: List[File]): List[File] = {
        files.filter( file => {
            if (file.isDirectory) false
            else {
                val indexEntry = IndexEntry.fromFile(repoFolder, file)
                index.isModified(indexEntry)
            }
        })
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Util.handleException( () => {
            val files = Util.pathsToFiles(args.paths)
            val blobsFolder = IOBlob.getBlobsFolder(repoFolder)
            val indexFile = getIndexFile(repoFolder)
            val index = read(indexFile)
            val stagedFiles = IndexEntries.fromFiles(repoFolder, files)
            val newIndex = index.addAll(stagedFiles)

            write(indexFile, newIndex)
            IOBlob.writeAll(blobsFolder, files)

            None
        })
    }

    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        ???
    }
}
