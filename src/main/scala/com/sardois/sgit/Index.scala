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

    @tailrec
    def removeAll(mapIndex: MapIndex, files: List[StagedFile]): Either[String, MapIndex] = {
        if (files == Nil) {
            return Right(mapIndex)
        }
        val file = files.head
        val path = file.relativePath
        if (!(mapIndex contains path)) {
            return Left(path + ": isn't tracked by sgit.")
        }
        val newMapIndex = mapIndex - path
        removeAll(newMapIndex, files.tail)
    }

    /** Return the list of file (by path) not tracked by the index */
    @tailrec
    def listUntrackedFiles(mapIndex: MapIndex, files: List[StagedFile], untrackedFiles: List[String] = Nil):
        List[String] = {

        if (files == Nil) return untrackedFiles
        val file = files.head
        val path = file.relativePath
        if (!mapIndex.isDefinedAt(path))
            listUntrackedFiles(mapIndex, files.tail, path :: untrackedFiles)
        else
            listUntrackedFiles(mapIndex, files.tail, untrackedFiles)
    }
}

object IOIndex {

    def getIndexFile(repoFolder: File): Either[String, File] = {
        try {
            val indexFile = repoFolder/Repository.getIndexPath()
            Right(indexFile)
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
    }

    /* The index file of the repository. It either return an
    *  error or a tuple containing the repository file and the map index.
    */
    def read(indexFile: File): Either[String, Index.MapIndex] = {
        val lines = indexFile.lines().toList
        val either = readRec(lines, Map())

        either match {
            case Left(error) => Left(error)
            case Right(map) => Right(map)
        }
    }

    @tailrec
    def readRec(lines: List[String], map: Type.MapIndex): Either[String, Type.MapIndex] = {
        if (lines == Nil) return Right(map)
        else {
            val split = lines.head.split(" ")
            if (split.length < 2) {
                return Left("An entry in the index file is invalid.")
            }

            val path = split(0)
            val sha = split(1)
            val newMap = Map((path -> sha)) ++ map

            readRec(lines.tail, newMap)
        }
    }

    @impure
    def write(indexFile: File, mapIndex: Type.MapIndex): Option[String] = {
        try {
            indexFile.clear()

            mapIndex.keys.foreach((path) => {
                val str = path + " " + mapIndex(path)
                indexFile.appendLine(str)
            })

            None
        } catch {
            case ex: IOException => Some(ex.getMessage)
        }
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val files = Util.pathsToFiles(args.paths)

        Util.handleIOException( () => {
            val either = for {
                indexFile <- getIndexFile(repoFolder)
                blobsFolder <- IOBlob.getBlobsFolder(repoFolder)

                mapIndex <- read(indexFile)

                stagedFiles <- Right(StagedFiles(repoFolder, files))
                newMapIndex <- Right(Index.addAll(mapIndex, stagedFiles))

                writeResult <- Util.optionToEither(write(indexFile, newMapIndex))
                either <- Util.optionToEither(IOBlob.writeAll(blobsFolder, files))
            } yield either

            Util.eitherToOption(either)
        })
    }

    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        ???
    }
}
