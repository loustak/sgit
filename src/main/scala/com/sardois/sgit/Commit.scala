package com.sardois.sgit

import java.time.LocalDate

import better.files.File

class Commit(
    val message: String,
    val index: Index,
    val parentCommitSha: String,
    val author: String,
    val date: LocalDate
        ) {

    def sha(): String = {
        val str = message + index.toString +
            author + date + parentCommitSha
        Util.shaString(str)
    }

    override def toString: String = {
        val list = List(message, author, date, parentCommitSha, index.sha())
        list.mkString(System.lineSeparator())
    }
}

object Commit {

    def apply(
         message: String,
         index: Index,
         parentCommitSha: String,
         author: String = "Anonymous",
         date: LocalDate = LocalDate.now
             ): Commit = {

        new Commit(message, index, parentCommitSha, author, date)
    }

    def rootCommitSha(): String = {
        Util.shaString("ROOT COMMIT")
    }
}

object IOCommit {

    def getCommitsFile(repoFolder: File): File = {
        repoFolder/Repository.getCommitsPath()
    }

    @impure
    def read(commitFolder: File, commitFile: File): Commit = {
        if (commitFile.exists) {
            throw new RuntimeException("Commit doesn't exists at " + commitFile.pathAsString)
        }

        ???
    }

    @impure
    def read(commitFolder: File, commitSha: String): Commit = {
        val commitFile = commitFolder/commitSha
        read(commitFolder, commitFile)
    }

    @impure
    def write(commitFolder: File, commit: Commit): Unit = {
        val commitSha = commit.sha()
        val commitFile = commitFolder/commitSha
        if (commitFile.exists) {
            throw new RuntimeException("Commit sha already exists")
        }

        commitFile.createFile()
        commitFile.write(commit.toString)
    }

    @impure
    def commit(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        Util.handleException( () => {
            val message = args.commitMessage
            val commitsFolder = getCommitsFile(repoFolder)
            val indexFile = IOIndex.getIndexFile(repoFolder)
            val index = IOIndex.read(indexFile)

            val checkableHeadFile = IOHead.getCheckableFile(repoFolder)
            val parentCommitSha = IOHead.getPointedCommitSha(repoFolder)
            val newCommit = Commit(message, index, parentCommitSha)

            write(commitsFolder, newCommit)

            // Save the new index file
            val indexFolder = IOIndex.getIndexesFolder(repoFolder)
            val newIndexFile = indexFolder/index.sha()
            IOIndex.write(newIndexFile, index)

            // Update the commit referenced by the head
            IOCheckable.setToSha(checkableHeadFile, newCommit.sha())

            None
        })
    }
}