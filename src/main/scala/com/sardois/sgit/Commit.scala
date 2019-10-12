package com.sardois.sgit

import java.io.IOException
import java.time.LocalDate

import better.files.File

class Commit(
    val message: String,
    val index: Index,
    val author: String,
    val date: LocalDate
        ) {

    def sha(): String = {
        val str = message + index.toString + author + date
        Util.shaString(str)
    }

    override def toString: String = {
        val newLine = System.lineSeparator()
        message + newLine + author + newLine + date + newLine + newLine + index.toString
    }
}

object Commit {

    def apply(
         message: String,
         index: Index,
         author: String = "Anonymous",
         date: LocalDate = LocalDate.now
             ): Commit = {

        new Commit(message, index, author, date)
    }
}

object IOCommit {

    def getCommitsFile(repoFolder: File): File = {
        repoFolder/Repository.getCommitsPath()
    }

    @impure
    def write(commitFolder: File, commit: Commit): Unit = {
        val commitSha = commit.sha()
        val commitFile = commitFolder/commitSha
        if (commitFile.exists) throw new RuntimeException("Commit sha already exists")

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

            val newCommit = Commit(message, index)
            write(commitsFolder, newCommit)

            None
        })
    }
}