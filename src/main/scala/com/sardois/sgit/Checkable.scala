package com.sardois.sgit

import better.files.File

trait Checkable {

    val name: String
    val commit: TCommit

    def getType(): String

    def getPath(): String

    override def toString(): String = {
        getType() + " " + name
    }
}

class Branch(val name: String, val commit: TCommit) extends Checkable {

    def getType(): String = {
        Branch.getType()
    }

    def getPath(): String = {
        Repository.getBranchesPath()
    }
}

object Branch {

    def apply(name: String, commit: TCommit): Branch = {
        new Branch(name, commit)
    }

    def getType(): String = {
        "branch"
    }

    def master(): Branch = {
        Branch("master", RootCommit)
    }
}

class Tag(val name: String, val commit: TCommit) extends Checkable {

    def getType(): String = {
        Tag.getType()
    }

    def getPath(): String = {
        Repository.getTagsPath()
    }
}

object Tag {

    def apply(name: String, commit: TCommit): Tag = {
        new Tag(name, commit)
    }

    def getType(): String = {
        "tag"
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