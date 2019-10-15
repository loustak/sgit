package com.sardois.sgit

import better.files._

object Head {

}

object IOHead {

    def getHeadFile(repoFolder: File): File = {
        repoFolder/Repository.headPath
    }

    @impure
    def read(headFile: File): (String, String) = {
        val line = headFile.lines.toArray

        if (!line.isDefinedAt(0)) {
            throw new RuntimeException("HEAD file is corrupted")
        }

        val split = line(0).split(" ")
        if (split.size != 2) {
            throw new RuntimeException("HEAD file format is invalid")
        }

        (split(0), split(1))
    }

    @impure
    def getCheckableFile(repoFolder: File): File = {
        val headFile = getHeadFile(repoFolder)
        val tuple = read(headFile)

        val checkableType = tuple._1
        val checkableName = tuple._2

        if (checkableType == Branch.getType) {
            val branchesFolder = IOBranch.getBranchesFolder(repoFolder)
            return branchesFolder/checkableName
        } else if (checkableType == Tag.getType) {
            val tagsFolder = IOTag.getTagsFolder(repoFolder)
            return tagsFolder/checkableName
        }

        throw new RuntimeException("HEAD as unknown checkable type")
    }

    @impure
    def getPointedCommitSha(repoFolder: File): String = {
        val checkableFile = getCheckableFile(repoFolder)
        val commitSha = checkableFile.contentAsString

        if (commitSha.length <= 0) {
            throw new RuntimeException("Invalid file format, no commit sha in file " + checkableFile.pathAsString)
        }

        commitSha
    }

    @impure
    def getPointedCommit(repoFolder: File): Commit = {
        val commitFolder = IOCommit.getCommitsFolder(repoFolder)
        val commitSha = getPointedCommitSha(repoFolder)
        IOCommit.read(commitFolder, commitSha)
    }

    @impure
    def getPointedIndex(repoFolder: File): Index = {
        val commit = getPointedCommit(repoFolder)

        if (commit.sha() == Commit.root.sha()) {
            return Index()
        }

        val indexSha = commit.indexSha
        val indexFolder = IOIndex.getIndexesFolder(repoFolder)
        IOIndex.read(indexFolder, indexSha)
    }

    @impure
    def write(repoFolder: File, checkable: Checkable): Unit = {
        val headFile = getHeadFile(repoFolder)
        headFile.clear()
        headFile.write(checkable.toString())
    }

    @impure
    def setToCommit(repoFolder: File, commit: Commit): Unit = {
        val checkableHeadFile = getCheckableFile(repoFolder)
        IOCheckable.setToSha(checkableHeadFile, commit.sha())
    }
}