package com.sardois.sgit

import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.util.{Calendar, Date}

import better.files.File

class Commit(
    val message: String,
    val indexSha: String,
    val parentCommitSha: String,
    val author: String,
    val dateString: String
        ) {

    def sha: String = {
        Util.shaString(toString)
    }

    def date: Date = {
        Commit.dateFormatter.parse(dateString)
    }

    override def toString: String = {
        val list = List(message, author, dateString, parentCommitSha, indexSha)
        list.mkString(System.lineSeparator())
    }
}

object Commit {

    def dateFormatter = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")

    def apply(
                 message: String,
                 indexSha: String,
                 parentCommitSha: String,
                 author: String = "Anonymous",
                 dateString: String = dateFormatter.format(Calendar.getInstance().getTime)
             ): Commit = {

        new Commit(message, indexSha, parentCommitSha, author, dateString)
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
        val indexFile = IOIndex.getIndexFile(repoFolder)
        val newIndex = IOIndex.getIndex(repoFolder)
        val oldIndex = IOHead.getOldIndex(repoFolder)
        IOIndex.clean(repoFolder, newIndex, oldIndex)

        val indexFolder = IOIndex.getIndexesFolder(repoFolder)
        val commitIndex = IOIndex.read(indexFolder, commit.indexSha)
        val map = commitIndex.getMap

        map.foreach( tuple => {
            val workingDirectoryBlobPath = tuple._1
            val blobSha = tuple._2

            val blobFile = IOBlob.getBlobFile(repoFolder, blobSha)
            val workingDirectoryBlobFile = repoFolder.parent/workingDirectoryBlobPath

            blobFile.copyTo(workingDirectoryBlobFile, true)
        })

        IOIndex.write(indexFile, commitIndex)
        IOHead.setToCommit(repoFolder, commit)

        // TODO: pass in detechached mode if we checkout a commit
    }

    @impure
    def checkout(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val checkableName = args.branchTagOrCommit
        val commitToCheckout = IOCheckable.find(repoFolder, checkableName)
        checkout(repoFolder, commitToCheckout)
        None
    }

    @impure
    def log(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val commitsFolder = getCommitsFolder(repoFolder)
        val commitsFiles = commitsFolder.list

        val commits = commitsFiles.map( file => {
            read(file)
        }).toList

        val sortedCommits = commits.sortWith( (c1, c2) => {
            c1.date.after(c2.date)
        })

        Some(sortedCommits.mkString(System.lineSeparator()))
    }
}