package com.sardois.sgit

import better.files.File

class Tag(val name: String, val commitSha: String) extends Checkable {

    def getType(): String = {
        Tag.getType()
    }

    def getPath(): String = {
        Repository.getTagsPath()
    }
}

object Tag {

    def apply(name: String, commitSha: String): Tag = {
        new Tag(name, commitSha)
    }

    def getType(): String = {
        "tag"
    }
}

object IOTag {

    def getTagsFolder(repoFolder: File): File = {
        repoFolder/Repository.getTagsPath()
    }
}