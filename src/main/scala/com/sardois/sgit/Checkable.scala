package com.sardois.sgit

import better.files.File

trait Checkable {

    val name: String
    val commitSha: String

    def getType(): String

    def getPath(): String

    override def toString(): String = {
        getType() + " " + name
    }
}

object IOCheckable {

    @impure
    def create(repoFolder: File, checkable: Checkable): Unit = {
        val checkableFile = repoFolder/checkable.getPath()/checkable.name

        if (checkableFile.exists) {
            throw new RuntimeException("This name is already used")
        }

        checkableFile.write(checkable.commitSha)
    }

    @impure
    def setToSha(checkableFile: File, newSha: String): Unit = {
        checkableFile.clear()
        checkableFile.write(newSha)
    }
}