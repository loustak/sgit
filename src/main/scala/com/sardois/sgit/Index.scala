package com.sardois.sgit


import better.files.File

import scala.annotation.tailrec

class StagedFile(val relativePath: String, val sha: String) {

}

object StagedFile {

    def apply(relativePath: String, sha: String): StagedFile = new StagedFile(relativePath, sha)

    def fromFile(repoFolder: File, file: File): StagedFile = {
        val relativePath = Repository.relativePathFromRepo(repoFolder, file)
        val sha = Util.shaFile(file)
        StagedFile(relativePath, sha)
    }
}

object StagedFiles {
    
    def apply(repoFolder: File, files: List[File]): List[StagedFile] = {
        files.map( file => StagedFile.fromFile(repoFolder, file))
    }
}

class Index(mapIndex: Map[String, String]) {

    def size(): Int = mapIndex.size

    def addLine(path: String, sha: String): Index = {
        Index(mapIndex + (path -> sha))
    }

    @tailrec
    final def addAll(files: List[StagedFile], index: Index = this): Index = {
        if (files == Nil) return index
        val file = files.head
        val path = file.relativePath
        val sha = file.sha
        val newIndex = addLine(path, sha)
        addAll(files.tail, newIndex)
    }

    override def toString: String = {
        mapIndex.keys.map( path => {
            path + " " + mapIndex(path)
        }).mkString(System.lineSeparator())
    }
}

object Index {

    def apply(mapIndex: Map[String, String] = Map()): Index = new Index(mapIndex)
}

object IOIndex {

    def getIndexFile(repoFolder: File): File = {
        repoFolder/Repository.getIndexPath()
    }

    def read(indexFile: File): Index = {
        val lines = indexFile.lines().toList
        readRec(lines, Index())
    }

    @tailrec
    def readRec(lines: List[String], index: Index): Index = {
        if (lines == Nil) return index
        else {
            val split = lines.head.split(" ")
            if (split.length < 2) {
                throw new IllegalArgumentException("An entry in the index file is invalid")
            }

            val path = split(0)
            val sha = split(1)

            val newIndex = index.addLine(path, sha)

            readRec(lines.tail, newIndex)
        }
    }

    @impure
    def write(indexFile: File, index: Index): Unit = {
        indexFile.clear()
        indexFile.write(index.toString)
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Util.handleException( () => {
            val files = Util.pathsToFiles(args.paths)
            val blobsFolder = IOBlob.getBlobsFolder(repoFolder)
            val indexFile = getIndexFile(repoFolder)
            val mapIndex = read(indexFile)
            val stagedFiles = StagedFiles(repoFolder, files)
            val newMapIndex = mapIndex.addAll(stagedFiles)

            write(indexFile, newMapIndex)
            IOBlob.writeAll(blobsFolder, files)

            None
        })
    }

    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        ???
    }
}
