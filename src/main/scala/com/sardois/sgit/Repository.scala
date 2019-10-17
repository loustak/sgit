package com.sardois.sgit

import better.files._

import scala.annotation.tailrec

object Repository {

    // TODO: change to sgit
    def directoryName: String = "vcs"

    def currentPath: String = {
        System.getProperty("user.dir")
    }

    def currentFolder: File = {
        File(currentPath)
    }

    /** If it's a sgit repository return the
     * repository path, else return None.
     */
    @tailrec
    final def isARepository(folder: File): Option[File] = {
        val repoFile = folder/directoryName
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
    def callInside(args: Config, func: (File, File, Config) => Option[String]): Option[String] = {
        val currentFolder = File(currentPath)
        val commandFolder = currentFolder

        Repository.isARepository(currentFolder) match {
            case Some(repoFolder) =>
                // Call the given function and handle some exceptions
                func(repoFolder, commandFolder, args)
            case _ => throw new RuntimeException("Not a sgit repository")
        }
    }

    def relativize(repoFolder: File, file: File): String = {
        repoFolder.parent.relativize(file).toString
    }

    def relativize(repoFolder: File, path: String): String = {
        path.replace(repoFolder.parent.pathAsString, "")
    }

    def relativizesPath(repoFolder: File, paths: Iterable[String]): Iterable[String] = {
        paths.map( path => {
            relativize(repoFolder, path)
        })
    }

    def relativizesFile(repoFolder: File, files: Iterable[File]): Iterable[String] = {
        files.map( file => {
            relativize(repoFolder, file)
        })
    }

    def isARepositoryFile(repoFolder: File, file: File): Boolean = {
        file.isChildOf(repoFolder) || file == repoFolder
    }

    /** List recursively all the files and folders inside
     * the parent folder of the repository.
     * Doesn't return files and folders inside the the .sgit directory
     * nor the repository parent directory.
     * */
    def list(repoFolder: File): Iterable[File] = {
        repoFolder.parent.list( file => {
            !(file.isDirectory || isARepositoryFile(repoFolder, file))
        }).toList
    }

    def detachedPath: String = "detached"

    def headPath: String = "HEAD"

    def indexPath: String = "index"

    def indexesPath: String = "indexes"

    def branchesPath: String = "branches"

    def tagsPath: String = "tags"

    def blobsPath: String = "blobs"

    def commitsPath: String = "commits"
}

object IORepository {

    @impure
    def init(folder: File): Either[String, File] = {
        Repository.isARepository(folder) match {
            case Some(_) => return Left("This is already an sgit repository.")
            case None =>
        }

        // Create the repository folder
        val repoFolder = folder/Repository.directoryName
        repoFolder.createDirectories()

        // Create the indexes folder
        val indexesFolder = repoFolder/Repository.indexesPath
        indexesFolder.createDirectories()

        // Create the blobs folder
        val blobsFolder = repoFolder/Repository.blobsPath
        blobsFolder.createDirectories()

        // Create the commits folder
        val commitsFolder = repoFolder/Repository.commitsPath
        commitsFolder.createDirectories()

        // Create the index file
        val index = repoFolder/Repository.indexPath
        index.createFile()

        // Write the checkables/branches and the master branch
        val heads = repoFolder/Repository.branchesPath
        heads.createDirectories()
        val master = Branch.master
        IOCheckable.create(repoFolder, master)

        // Write the head
        IOHead.write(repoFolder, master)

        // Create the tags folder
        val tags = repoFolder/Repository.tagsPath
        tags.createDirectories()

        Right(repoFolder)
    }
}