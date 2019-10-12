package com.sardois.sgit

import better.files.File

trait Checkable {

    val name: String
    val commit: TCommit

    def getPath(): String
}

class Branch(val name: String, val commit: TCommit) extends Checkable {

    def getPath(): String = {
        Repository.getBranchesPath()
    }
}

object Branch {

    def apply(name: String, commit: TCommit): Branch = {
        new Branch(name, commit)
    }

    def master(): Branch = {
        Branch("master", RootCommit)
    }
}

class Tag(val name: String, val commit: TCommit) extends Checkable {

    def getPath(): String = {
        Repository.getTagsPath()
    }
}

object Tag {

    def apply(name: String, commit: TCommit): Tag = {
        new Tag(name, commit)
    }
}

object IOCheckable {

    def write(repoFolder: File, checkable: Checkable): Unit = {
        val checkableFile = repoFolder/checkable.getPath()/checkable.name

        if (checkableFile.exists) {
            checkableFile.clear()
        }

        checkableFile.write(checkable.commit.sha())
    }
}