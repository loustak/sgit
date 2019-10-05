package com.lucassardois

import better.files._

/* Used to get a path to the repository in real conditions */
object RepositoryReal {

    def getName(): String = ".sgit"

    def getCurrentFolder(): String = {
        System.getProperty("user.dir")
    }

    def getRepositoryPath(): String = {
        getCurrentFolder() + "/" + getName()
    }
}

object Repository {

    /* Return wether or not a sgit repository
        exists at the given path */
    def existsAt(path: String): Boolean = {
        val file = File(path)
        return file.isDirectory()
    }

    def init(path: String): Either[String, Repository] = {
        if (existsAt(path)) {
            return Left("This is already an sgit repository.")
        }

        val master = new Branch("master", NoParentCommit)

        Right(new Repository(path, master))
    }

    def initFromFolder(path: String): Repository = ???
}

class Repository(val path: String, val head: Branch) {

    /* Delete all the files in a repository, this functions
    is not available for the user but only for tests this function
    is unpure since it only manage files */
    def _delete(): Unit = {
        val file = File(path)
        file.delete()
    }

    /* Unpure function wich write the repository to the disk */
    def _write(): Unit = {
        val file = File(path)
        file.createDirectory()
    }
}