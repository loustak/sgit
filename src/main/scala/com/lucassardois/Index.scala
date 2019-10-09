package com.lucassardois

import java.io.{FileNotFoundException, IOException}

import better.files.File

import scala.annotation.tailrec

object StagedFile {

    def createFromPath(repoFolder: File, path: String): Index.StagedFile = {
        val file = File(path)
        val relativePath = Repository.relativePathFromRepo(repoFolder, file)
        val sha = file.sha256
        (relativePath, sha)
    }

    def createAllFromPath(repoFolder: File, paths: List[String]): Either[String, List[Index.StagedFile]] = {
        try {
            val indexedFiles = paths.map((path) => {
                createFromPath(repoFolder, path)
            })
            Right(indexedFiles)
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
}

object IOIndex {

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

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Chain(read(repoFolder), (mapIndex: Type.MapIndex) => {
            Chain(StagedFile.createAllFromPath(repoFolder, args.paths), (filesToAdd: List[Index.StagedFile]) => {
                val newIndex = Index.addAll(mapIndex, filesToAdd)
                write(repoFolder, newIndex)
            })
        })
    }

    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Chain(read(repoFolder), (mapIndex: Type.MapIndex) => {
            Chain(StagedFile.createAllFromPath(repoFolder, args.paths), (filesToRemove: List[Index.StagedFile]) => {
                Chain(Index.removeAll(mapIndex, filesToRemove), (newIndex: Type.MapIndex) => {
                    println(newIndex)
                    write(repoFolder, newIndex)
                })
            })
        })
    }

    /*
    def status(repoFolder: File, commandFolder: File, args: Config): Option[String] = ???
    */
}
