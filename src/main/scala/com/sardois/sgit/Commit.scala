package com.sardois.sgit

import java.text.SimpleDateFormat
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.util.{Calendar, Date}

import better.files.File
import com.sardois.sgit.IOIndex.throwIfUncommitedChanges

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

    // TODO: Be able to read multi line message
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

        // TODO: check that there is changes to commit
        val parentCommitSha = IOHead.getPointedCommitSha(repoFolder)
        val newCommit = Commit(message, indexSha, parentCommitSha)

        write(commitsFolder, newCommit)

        // Save the new index file
        val indexFolder = IOIndex.getIndexesFolder(repoFolder)
        val newIndexFile = indexFolder/indexSha
        IOIndex.write(newIndexFile, index)

        // Update the commit referenced by the head
        IOHead.setToCommit(repoFolder, newCommit)
        val detachedFile = IOHead.getDetachedFile(repoFolder)
        if (IOHead.isDetached(detachedFile)) {
            IOHead.detach(detachedFile, newCommit.sha)
        }

        None
    }

    @impure
    def checkout(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        // TODO: check that we are not on the checkable we try to reach
        val checkableName = args.branchTagOrCommit
        val tuple = IOCheckable.find(repoFolder, checkableName)
        val toDetach = tuple._1
        val commitToCheckout = tuple._2

        val indexFolder = IOIndex.getIndexesFolder(repoFolder)
        val commitIndex = IOIndex.read(indexFolder, commitToCheckout.indexSha)
        val commitIndexMap = commitIndex.getMap
        val indexFile = IOIndex.getIndexFile(repoFolder)
        val newIndex = IOIndex.getIndex(repoFolder)
        val oldIndex = IOHead.getOldIndex(repoFolder)

        val files = Repository.list(repoFolder)
        val paths = Util.filesToPath(files)
        throwIfUncommitedChanges(repoFolder, newIndex, oldIndex, paths)

        val untrackedFiles = IOIndex.getUntrackedFiles(repoFolder, newIndex, paths)
        val untrackedFilesConflict = commitIndexMap.filter( line => {
            val path = line._1
            untrackedFiles.toList.contains(path)
        })

        if (untrackedFilesConflict.size > 0) {
            val conflictsPath = untrackedFilesConflict.keys.map( path => path).mkString(", ")
            throw new RuntimeException("The following untracked files conflicts: " + conflictsPath)
        }

        IOIndex.clean(repoFolder, newIndex)

        commitIndexMap.foreach( tuple => {
            val workingDirectoryBlobPath = tuple._1
            val blobSha = tuple._2

            val blobFile = IOBlob.getBlobFile(repoFolder, blobSha)
            val workingDirectoryBlobFile = repoFolder.parent/workingDirectoryBlobPath

            blobFile.copyTo(workingDirectoryBlobFile, true)
        })

        IOIndex.write(indexFile, commitIndex)

        val detachedFile = IOHead.getDetachedFile(repoFolder)
        if (toDetach) {
            IOHead.detach(detachedFile, commitToCheckout.sha)
            IOHead.setToCommit(repoFolder, commitToCheckout)
        } else {
            IOHead.attach(detachedFile)
            IOHead.setToCommit(repoFolder, commitToCheckout)
        }

        None
    }

    @impure
    def log(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        // TODO: Only show the log of the current branch
        val commitsFolder = getCommitsFolder(repoFolder)
        val commitsFiles = commitsFolder.list

        val commits = commitsFiles.map( file => {
            read(file)
        }).toList

        val sortedCommits = commits.sortWith( (c1, c2) => {
            c1.date.after(c2.date)
        })

        val newLine = System.lineSeparator()
        Some(sortedCommits.mkString(newLine + newLine))
    }
}