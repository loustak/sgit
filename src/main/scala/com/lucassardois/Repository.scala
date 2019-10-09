package com.lucassardois

import better.files._
import scala.annotation.tailrec

object Repository {

    def getDirectoryName(): String = ".sgit"

    def getCurrentPath(): String = {
        System.getProperty("user.dir")
    }

    def getCurrentFolder(): File = {
        File(getCurrentPath())
    }

    def getRepositoryPath(): String = {
        getCurrentPath() + "/" + getDirectoryName()
    }

    /** Return wether or not a sgit repository
     *  exists at the given path if not, check for parent
     */
    @tailrec
    final def isARepository(folder: File): Option[File] = {
        val repoFile = folder/getDirectoryName()
        if (repoFile.isDirectory()) {
            Some(repoFile)
        } else {
            val parent = folder.parent
            if (parent == null) return None
            isARepository(parent)
        }
    }

    /** Call a function inside a repository, if the repository isn't a
     *  valid sgit repository it returns an error as a string.
     *  It also inject the path of the issued command as a function parameter.
     */
    def callInside(
        args: Config,
        func: (File, File, Config) => Option[String]): 
            Option[String] = {

        val currentFolder = File(getCurrentPath())
        val commandFolder = currentFolder

        Repository.isARepository(currentFolder) match {
            case Some(repoFolder) => func(repoFolder, commandFolder, args)
            case _ => Some("Not a sgit repository.")
        }
    }

    def pathFromRepo(repoFolder: File, file: File): String = {
        file.pathAsString.replace(repoFolder.pathAsString + "/", "")
    }

    def getRefsPath(): String = "refs"

    def getHeadsPath(): String = getRefsPath() + "/heads"

    def getHeadPath(): String = "HEAD"

    def getIndexPath(): String = "index"
}

object IORepository {

    @impure
    def init(folder: File): Either[String, File] = {
        Repository.isARepository(folder) match {
            case Some(_) => return Left("This is already an sgit repository.")
            case None =>
        }

        val master = new Branch("master", NoParentCommit)
        val branches = List(master)

        // Create the repository folder
        val repoFolder = folder/Repository.getDirectoryName()
        repoFolder.createDirectories()

        // Create the index file
        val index = repoFolder/Repository.getIndexPath()
        index.createFile()

        // Write the head
        val head = repoFolder/Repository.getHeadPath()
        head.write(master.name.toString())

        // Write the ref/heads and branches
        val heads = repoFolder/Repository.getHeadsPath()
        heads.createDirectories()
        IOBranch.writeAll(heads, branches)

        Right(repoFolder)
    }

    /** Try to read the index file of the repository. It either return an
     *  error or a typle containing the repository file and the map index.
     */
    def readIndex(repoFolder: File): Either[String, (File, Type.MapIndex)] = {
        val indexFile = repoFolder/Repository.getIndexPath()

        if (!indexFile.exists) {
            return Left("Repository is corrupt: index file doesn't exists.")
        }

        val lines = indexFile.lines().toList
        val either = readIndexRec(lines, Map())

        either match {
            case Left(error) => Left(error)
            case Right(map) => {
                Right((indexFile, map))
            }
        }
    }

    @tailrec
    def readIndexRec(lines: List[String], map: Type.MapIndex): Either[String, Type.MapIndex] = {
        if (lines == Nil) return Right(map)
        else {
            val split = lines.head.split(" ")
            if (split.length < 2) {
                return Left("Index file is invalid.")
            }

            val path = split(0)
            val sha = split(1)
            val newMap = Map((path -> sha)) ++ map

            readIndexRec(lines.tail, newMap) 
        }
    }

    @impure
    def writeIndex(repoFolder: File, mapIndex: Type.MapIndex): Unit = {
        val indexFile = repoFolder/Repository.getIndexPath()
        indexFile.clear() 

        mapIndex.keys.foreach( (path) => {
            val str = path + " " + mapIndex(path)
            indexFile.appendLine(str)
        })
    }

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        readIndex(repoFolder) match {
            case Left(error) => Some(error)
            case Right(value) => {

                val files = args.paths.map( (path) => File(path))
                val mapIndex = files.map( (file) => {
                    File(file)
                })

                writeIndex(repoFolder, mapIndex)

                None
            }
        }
    }

    def addRec(repoFolder: File, files: List[File]): Type.MapIndex = {
        if (files == Nil) return Nil

        val file = files.head
        val path = Repository.pathFromRepo(repoFolder, file)

        addRec(repoFolder, files.tail)
    }

    @impure
    def remove(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val either = readIndex(repoFolder) 
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

                writeIndex(repoFolder, newMap)

                None
            }
        }
    }

    def status(repoFolder: File, commandFolder: File, args: Config): Option[String] = ???
}