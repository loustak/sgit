package com.sardois.sgit

import java.io.IOException
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

object Index {

    type MapIndex = Map[String, String]

    @tailrec
    def addAll(mapIndex: Type.MapIndex, files: List[StagedFile]): MapIndex = {
        if (files == Nil) return mapIndex
        val file = files.head
        val newMapIndex = mapIndex + (file.relativePath -> file.sha)
        addAll(newMapIndex, files.tail)
    }
}

object IOIndex {

    def getIndexFile(repoFolder: File): File = {
        repoFolder/Repository.getIndexPath()
    }

    /* The index file of the repository. It either return an
    *  error or a tuple containing the repository file and the map index.
    */
    def read(indexFile: File): Index.MapIndex = {
        val lines = indexFile.lines().toList
        readRec(lines, Map())
    }

    @tailrec
    def readRec(lines: List[String], map: Type.MapIndex): Type.MapIndex = {
        if (lines == Nil) return map
        else {
            val split = lines.head.split(" ")
            if (split.length < 2) {
                throw new IllegalArgumentException("An entry in the index file is invalid")
            }

            val path = split(0)
            val sha = split(1)
            val newMap = Map((path -> sha)) ++ map

            readRec(lines.tail, newMap)
        }
    }

    @impure
    def write(indexFile: File, mapIndex: Type.MapIndex): Unit = {
        indexFile.clear()

        mapIndex.keys.foreach((path) => {
            val str = path + " " + mapIndex(path)
            indexFile.appendLine(str)
        })
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Util.handleException( () => {
            val files = Util.pathsToFiles(args.paths)
            val blobsFolder = IOBlob.getBlobsFolder(repoFolder)
            val indexFile = getIndexFile(repoFolder)
            val mapIndex = read(indexFile)
            val stagedFiles = StagedFiles(repoFolder, files)
            val newMapIndex = Index.addAll(mapIndex, stagedFiles)

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
