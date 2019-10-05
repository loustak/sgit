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

object RepositoryConstructor {
    def apply(path: String): Repository = {
        val file = File(path)

        val master = new Branch("master", NoParentCommit)

        if (file.isDirectory()) {
            // TODO: How to handle the error?
            println("existe")
        } else {
            file.createDirectory()
        }

        new Repository(path, master)
    }
}

class Repository(val path: String, val head: Branch) {
}