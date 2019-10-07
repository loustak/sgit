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

    /* Return wether or not a sgit repository
        exists at the given path if not, check for parent */
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

    /* Call a function inside a repository, if the repository isn't a
    valid sgit repository it returns an error as a string.
    It also inject the path of the issued command as a function parameter. */
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

    @impure
    def add(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val indexFile = repoFolder/Repository.getIndexPath()

        if (!indexFile.exists) {
            return Some("Repository is corrupt: index file doesn't exists.")
        }

        val files = args.paths.map( (path) => File(path))

        files.foreach( (file) => {
            if (file.isRegularFile) {
                val str = file.sha256 + " " + Repository.pathFromRepo(repoFolder.parent, file)
                indexFile.append(str)
            }
        })

        None
    }
}