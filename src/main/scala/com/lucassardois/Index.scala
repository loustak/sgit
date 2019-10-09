package com.lucassardois

import java.io.{FileNotFoundException, IOException}

import better.files.File

import scala.annotation.tailrec

object IndexedFile {

    def createFromPath(repoFolder: File, path: String): (String, String) = {
        val file = File(path)
        val relativePath = Repository.relativePathFromRepo(repoFolder, file)
        val sha = file.sha256
        (relativePath, sha)
    }

    def pathsToIndexedFile(repoFolder: File, paths: List[String]): Either[String, List[(String, String)]] = {
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

    @tailrec
    def addAll(mapIndex: Type.MapIndex, files: List[(String, String)]): Type.MapIndex = {
        if (files == Nil) return mapIndex
        val file = files.head
        val newMapIndex = mapIndex + (file._1 -> file._2)
        addAll(newMapIndex, files.tail)
    }
}

object IOIndex {

    /* The index file of the repository. It either return an
    *  error or a tuple containing the repository file and the map index.
    */
    def read(repoFolder: File): Either[String, Map[String, String]] = {
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

    def readAndIndex(repoFolder: File, pathsToIndex: List[String]): Either[String, List[(String, String)]] = {
        read(repoFolder) match {
            case Left(error) => Left(error)
            case Right(mapIndex) => {
                IndexedFile.pathsToIndexedFile(repoFolder, pathsToIndex) match {
                    case Left(error) => Left(error)
                    case Right(indexedFiles) => Right(indexedFiles)
                }
            }
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

    def chainEither[T](either: Either[String, T], func: (T) => Option[String]): Option[String] = {
        either match {
            case Left(error) => Some(error)
            case Right(value) => func(value)
        }
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Chain(read(repoFolder), (mapIndex: Type.MapIndex) => {
            Chain(IndexedFile.pathsToIndexedFile(repoFolder, args.paths), (filesToIndex: List[(String, String)]) => {
                val newIndex = Index.addAll(mapIndex, filesToIndex)
                write(repoFolder, newIndex)
            })
        })
    }

    /*
    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val either = read(repoFolder)
        either match {
            case Left(error) => Some(error)
            case Right(value) => {
                val indexFile = value._1
                val indexMap = value._2

                val paths = args.paths

                println(indexMap.toString())
                val newMap = indexMap.filter( (tuple) => {
                    !(paths contains tuple._2)
                })
                println(newMap.toString())

                write(repoFolder, newMap)

                None
            }
        }
    }

    def status(repoFolder: File, commandFolder: File, args: Config): Option[String] = ???
    */
}
