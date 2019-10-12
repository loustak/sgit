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

    def apply(files: List[(String, String)]): List[StagedFile] = {
        files.map( file => StagedFile(file._1, file._2))
    }
    
    def fromFiles(repoFolder: File, files: List[File]): List[StagedFile] = {
        files.map( file => StagedFile.fromFile(repoFolder, file))
    }
}

class Index(map: Map[String, String]) {

    def size: Int = map.size

    def addLine(path: String, sha: String): Index = {
        Index(map + (path -> sha))
    }

    def add(file: StagedFile): Index = {
        val path = file.relativePath
        val sha = file.sha
        val t = addLine(path, sha)
        t
    }

    def addAll(stagedFiles: List[StagedFile]): Index = {

        @tailrec
        def rec(stagedFiles: List[StagedFile], index: Index): Index = {
            stagedFiles match {
                case ::(head, next) => rec(next, index.add(head))
                case Nil => index
            }
        }

        rec(stagedFiles, this)
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

    def getIndexFile(repoFolder: File): File = {
        repoFolder/Repository.getIndexPath()
    }

    def read(indexFile: File): Index = {
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
            val index = read(indexFile)
            val stagedFiles = StagedFiles.fromFiles(repoFolder, files)
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
