package com.sardois.sgit

import java.time.LocalDate

import better.files.File

class Commit(
    val message: String,
    val indexSha: String,
    val parentCommitSha: String,
    val author: String,
    val date: String
        ) {

    def sha: String = {
        Util.shaString(toString)
    }

    override def toString: String = {
        val list = List(message, author, date, parentCommitSha, indexSha)
        list.mkString(System.lineSeparator())
    }
}

object Commit {

    def apply(
         message: String,
         indexSha: String,
         parentCommitSha: String,
         author: String = "Anonymous",
         date: String = LocalDate.now.toString
             ): Commit = {

        new Commit(message, indexSha, parentCommitSha, author, date)
    }

    def root: Commit = {
        new Commit("Root commit", Index().sha(), "", "", "")
    }
}

object IOCommit {

    def getCommitsFolder(repoFolder: File): File = {
        repoFolder/Repository.commitsPath
    }

    @impure
    def read(commitFile: File): Commit = {
        if (!commitFile.exists) {
            throw new RuntimeException("Commit doesn't exists at " + commitFile.pathAsString)
        }

        val lines = commitFile.lines.toArray
        if (lines.length != 5) {
            throw new RuntimeException("Commit format is invalid at " + commitFile.pathAsString)
        }

        val message = lines(0)
        val author = lines(1)
        val date = lines(2)
        val parentCommitSha = lines(3)
        val indexSha = lines(4)

        Commit(message, indexSha, parentCommitSha, author, date)
    }

    @impure
    def read(commitFolder: File, commitSha: String): Commit = {
        if (commitSha == Commit.root.sha) {
            return Commit.root
        }

        val commitFile = commitFolder/commitSha
        read(commitFile)
    }

    @impure
    def write(commitFolder: File, commit: Commit): Unit = {
        val commitSha = commit.sha
        val commitFile = commitFolder/commitSha
        if (commitFile.exists) {
            throw new RuntimeException("Commit sha already exists")
        }

        commitFile.createFile()
        commitFile.write(commit.toString)
    }

    @impure
    def commit(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val message = args.commitMessage
        val commitsFolder = getCommitsFolder(repoFolder)
        val indexFile = IOIndex.getIndexFile(repoFolder)
        val index = IOIndex.read(indexFile)
        val indexSha = index.sha()

        val parentCommitSha = IOHead.getPointedCommitSha(repoFolder)
        val newCommit = Commit(message, indexSha, parentCommitSha)

        write(commitsFolder, newCommit)

        // Save the new index file
        val indexFolder = IOIndex.getIndexesFolder(repoFolder)
        val newIndexFile = indexFolder/indexSha
        IOIndex.write(newIndexFile, index)

        // Update the commit referenced by the head
        IOHead.setToCommit(repoFolder, newCommit)

        None
    }

    @impure
    def checkout(repoFolder: File, commit: Commit): Unit = {
        val headIndex = IOHead.getPointedIndex(repoFolder)
        IOIndex.clean(repoFolder, headIndex)

        val indexFolder = IOIndex.getIndexesFolder(repoFolder)
        val index = IOIndex.read(indexFolder, commit.indexSha)
        val map = index.getMap

        map.foreach( tuple => {
            val workingDirectoryBlobPath = tuple._1
            val blobSha = tuple._2

            val blobFile = IOBlob.getBlobFile(repoFolder, blobSha)
            val workingDirectoryBlobFile = repoFolder.parent/workingDirectoryBlobPath

            blobFile.copyTo(workingDirectoryBlobFile, true)
        })

        IOHead.setToCommit(repoFolder, commit)
    }

    @impure
    def checkout(repoFolder: File, commandFolder: File, args: Config): Unit = {
        ???
    }
}