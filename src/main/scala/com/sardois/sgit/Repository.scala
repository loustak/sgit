package com.sardois.sgit

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

    /** If it's a sgit repository return the
     * repository path, else return None.
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

    def relativePathFromRepo(repoFolder: File, file: File): String = {
        file.pathAsString.replace(repoFolder.parent.pathAsString + "/", "")
    }

    /** List recursively all the files and folders inside
     * the parent folder of the repository.
     * Doesn't return files and folders inside the the .sgit directory
     * nor the repository parent directory.
     * */
    def list(repoFolder: File): List[File] = {
        repoFolder.parent.list( (file) => {
            !(file.isChildOf(repoFolder) || file == repoFolder.parent || file == repoFolder)
        }).toList
    }

    def getRefsPath(): String = "refs"

    def getHeadPath(): String = "HEAD"

    def getIndexPath(): String = "index"

    def getHeadsPath(): String = getRefsPath() + "/heads"

    def getTagsPath(): String = getRefsPath() + "/tags"

    def getBlobsPath(): String = "blobs"

    def getCommitsPath(): String = "commits"
}

object IORepository {

    @impure
    def init(folder: File): Either[String, File] = {
        Repository.isARepository(folder) match {
            case Some(_) => return Left("This is already an sgit repository.")
            case None =>
        }

        // val master = new Branch("master", NoParentCommit)
        // val branches = List(master)

        // Create the repository folder
        val repoFolder = folder/Repository.getDirectoryName()
        repoFolder.createDirectories()

        // Create the blobs folder
        val blobsFolder = repoFolder/Repository.getBlobsPath()
        blobsFolder.createDirectories()

        // Create the commits folder
        val commitsFolder = repoFolder/Repository.getCommitsPath()
        commitsFolder.createDirectories()

        // Create the index file
        val index = repoFolder/Repository.getIndexPath()
        index.createFile()

        // Write the head
        val head = repoFolder/Repository.getHeadPath()
        // head.write(master.name.toString())

        // Write the ref/heads and branches
        val heads = repoFolder/Repository.getHeadsPath()
        heads.createDirectories()
        // IOBranch.writeAll(heads, branches)

        // Create the tags folder
        val tags = repoFolder/Repository.getTagsPath()
        tags.createDirectories()

        Right(repoFolder)
    }

    /* Returns true if the repository state is not equals
     * to the index.
     */
    def isDirty(repoFolder: File): Boolean = {
        // TODO: Make this function
        true
    }
}