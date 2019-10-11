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



}

object Commit {

    def apply(message: String, index: Index, author: String = "Anonymous", date: LocalDate = LocalDate.now): Commit = {
        new Commit(message, index, author, date)
    }
}

object IOCommit {

    def getCommitsFile(repoFolder: File): File = {
        repoFolder/Repository.getCommitsPath()
    }

    @impure
    def write(commitFolder: File, commit: Commit): Unit = {
    }

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