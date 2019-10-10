package com.sardois.sgit

import java.io.IOException

import better.files.File

import scala.annotation.tailrec

object StagedFile {

    def createFromFile(repoFolder: File, file: File): Index.StagedFile = {
        val relativePath = Repository.relativePathFromRepo(repoFolder, file)
        val sha = Util.shaFile(file)
        (relativePath, sha)
    }

    def createAllFromFiles(repoFolder: File, files: List[File]): Either[String, List[Index.StagedFile]]= {
        try {
            val filteredFiles = files.filter( (file) => {
                !file.isDirectory
            })

            val indexedFiles = filteredFiles.map( (file) => {
                createFromFile(repoFolder, file)
            })

            Right(indexedFiles)
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
    }

    def createFromPath(repoFolder: File, path: String): Index.StagedFile = {
        val file = File(path)
        createFromFile(repoFolder, file)
    }

    def createAllFromPath(repoFolder: File, paths: List[String]): Either[String, List[Index.StagedFile]] = {
        try {
            val files = paths.map( (path) => {
                File(path)
            })

            createAllFromFiles(repoFolder, files)
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
    }
}

object Index {

    type StagedFile = (String, String)

    type MapIndex = Map[String, String]

    @tailrec
    def addAll(mapIndex: Type.MapIndex, files: List[StagedFile]): MapIndex = {
        if (files == Nil) return mapIndex
        val file = files.head
        val newMapIndex = mapIndex + (file._1 -> file._2)
        addAll(newMapIndex, files.tail)
    }

    @tailrec
    def removeAll(mapIndex: MapIndex, files: List[StagedFile]): Either[String, MapIndex] = {
        if (files == Nil) return Right(mapIndex)
        val file = files.head
        val path = file._1
        if (!(mapIndex contains path)) {
            return Left(path + ": isn't tracked by sgit.")
        }
        val newMapIndex = mapIndex - path
        removeAll(newMapIndex, files.tail)
    }

    /** Return the list of file (by path) not tracked by the index */
    @tailrec
    def listUntrackedFiles(mapIndex: MapIndex, files: List[Index.StagedFile], untrackedFiles: List[String] = Nil):
        List[String] = {

        if (files == Nil) return untrackedFiles
        val file = files.head
        val path = file._1
        if (!mapIndex.isDefinedAt(path))
            listUntrackedFiles(mapIndex, files.tail, path :: untrackedFiles)
        else
            listUntrackedFiles(mapIndex, files.tail, untrackedFiles)
    }

    /** Return 3 map indexes, changes to be committed, changes not staged, untracked files */
    def status(mapIndex: MapIndex, files: List[StagedFile]): (MapIndex, MapIndex, List[File]) = {
        ???
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
    def read(repoFolder: File): Either[String, Index.MapIndex] = {
        val indexFile = repoFolder/Repository.getIndexPath()

        if (!indexFile.exists) {
            return Left("Repository is corrupt: index file doesn't exists.")
        }

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
    def write(repoFolder: File, mapIndex: Type.MapIndex): Option[String] = {
        try {
            val indexFile = repoFolder / Repository.getIndexPath()
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

    implicit  class ChainableEither[A, B](e: Either[A, B]) {
        def next[C, D](func: (B) => Option[A]): Option[A] = {
            e match {
                case Left(error) => Some(error)
                case Right(value) => func(value)
            }
        }
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {



        Chain(read(repoFolder), (mapIndex: Type.MapIndex) => {
            Chain(StagedFile.createAllFromPath(repoFolder, args.paths), (stagedFilesToAdd: List[Index.StagedFile]) => {
                val newIndex = Index.addAll(mapIndex, stagedFilesToAdd)
                write(repoFolder, newIndex)

                /*
                val files = stagedFilesToAdd.map( (stagedFile) => {
                    File(stagedFile._2)
                })
                Chain(IOBlob.getBlobsFolder(repoFolder), (blobsFolder: File) => {
                    IOBlob.writeAll(blobsFolder, files)
                })
                */
            })
        })
    }

    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Chain(read(repoFolder), (mapIndex: Type.MapIndex) => {
            Chain(StagedFile.createAllFromPath(repoFolder, args.paths), (filesToRemove: List[Index.StagedFile]) => {
                Chain(Index.removeAll(mapIndex, filesToRemove), (newIndex: Type.MapIndex) => {
                    write(repoFolder, newIndex)
                })
            })
        })
    }

    @impure
    def status(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Chain(read(repoFolder), (mapIndex: Type.MapIndex) => {
            Chain(StagedFile.createAllFromPath(repoFolder, args.paths), (filesToStatus: List[Index.StagedFile]) => {
                ???
            })
        })
    }

    /*
    def status(repoFolder: File, commandFolder: File, args: Config): Option[String] = ???
    */
}
