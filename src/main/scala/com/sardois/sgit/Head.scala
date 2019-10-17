package com.sardois.sgit

import better.files._

object IOHead {

    def getHeadFile(repoFolder: File): File = {
        repoFolder/Repository.headPath
    }

    def getDetachedFile(repoFolder: File) = {
        repoFolder/Repository.detachedPath
    }

    @impure
    def isDetached(detachedFile: File): Boolean = {
        detachedFile.exists
    }

    @impure
    def detach(detachedFile: File, commitSha: String): Unit = {
        detachedFile.write(commitSha)
    }

    @impure
    def attach(detachedFile: File): Unit = {
        if (detachedFile.exists) {
            detachedFile.delete()
        }
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

        if (checkableType == CheckableType.BRANCH.toString) {
            val branchesFolder = IOCheckable.getBranchesFolder(repoFolder)
            return branchesFolder/checkableName
        } else if (checkableType == CheckableType.TAG.toString) {
            val tagsFolder = IOCheckable.getTagsFolder(repoFolder)
            return tagsFolder/checkableName
        }

        throw new RuntimeException("The head as an unknown checkable type")
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
    def getPreviousCommit(repoFolder: File): Commit = {
        val commitFolder = IOCommit.getCommitsFolder(repoFolder)
        val commitSha = getPointedCommitSha(repoFolder)
        IOCommit.read(commitFolder, commitSha)
    }

    @impure
    def getOldIndex(repoFolder: File): Index = {
        val commit = getPreviousCommit(repoFolder)

        if (commit.sha == Commit.root.sha) {
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
        IOCheckable.setToSha(checkableHeadFile, commit.sha)
    }
}