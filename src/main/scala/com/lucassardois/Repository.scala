package com.lucassardois

import better.files._
import scala.annotation.tailrec

object Repository {

    def getDirectoryName(): String = ".sgit"

    def getCurrentPath(): String = {
        System.getProperty("user.dir")
    }

    def getRepositoryPath(): String = {
        getCurrentPath() + "/" + getDirectoryName()
    }

    /* Return wether or not a sgit repository
        exists at the given path if not, check for parent */
    @tailrec
    final def isARepository(folder: File): Boolean = {
        val repoFile = folder/getDirectoryName()
        if (repoFile.isDirectory()) {
            return true
        } else {
            val parent = folder.parent
            if (parent == null) return false
            isARepository(parent)
        }
    }

    def getRefsPath(): String = "refs"

    def getHeadsPath(): String = getRefsPath() + "/heads"

    def getHeadPath(): String = "HEAD"

    def getIndexPath(): String = "index"
}

object IORepository {

    @impure
    def init(folder: File): Either[String, File] = {
        if (Repository.isARepository(folder)) {
            return Left("This is already an sgit repository.")
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
}