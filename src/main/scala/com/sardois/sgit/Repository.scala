package com.sardois.sgit

import java.io.IOException

import better.files._

import scala.annotation.tailrec

case class Repository(repositoryFolder: File) {

    val workingDirectory: File = repositoryFolder.parent

    val headFile: File = repositoryFolder/Repository.headPath
    val indexFile: File = repositoryFolder/Repository.indexPath
    val branchesFolder: File = repositoryFolder/Repository.branchesPath
    val tagsFolder: File = repositoryFolder/Repository.tagsPath
    val blobsFolder: File = repositoryFolder/Repository.blobsPath
    val commitsFolder: File = repositoryFolder/Repository.commitsPath
    val indexesFolder: File = repositoryFolder/Repository.indexesPath

    @impure
    lazy val head: Either[String, Head] = {
        IO.read(this, headFile, Head.deserialize)
    }

    @impure
    lazy val branch: Either[String, Branch] = {
        for {
            head <- head
            branch <- head.branch
        } yield branch
    }

    @impure
    lazy val notCommitedCurrentIndex: Either[String, NotCommitedIndex] = {
        IO.read(this, indexFile, NotCommitedIndex.deserialize)
    }

    @impure
    lazy val commits: Either[String, List[Commit]] = {
        IO.readAll(this, commitsFolder, Commit.deserialize)
    }

    @impure
    lazy val branches: Either[String, List[Branch]] = {
        IO.readAll(this, branchesFolder, Branch.deserialize)
    }

    @impure
    lazy val tags: Either[String, List[Tag]] = {
        IO.readAll(this, tagsFolder, Tag.deserialize)
    }

    @impure
    lazy val blobs: Either[String, List[Blob]] = {
        IO.readAll(this, blobsFolder, Blob.deserialize)
    }

    @impure
    lazy val indexes: Either[String, List[CommitedIndex]] = {
        IO.readAll(this, indexesFolder, CommitedIndex.deserialize)
    }

    @impure
    lazy val lastCommitSha: Either[String, String] = {
        for {
            branch <- branch
        } yield branch.commitSha
    }

    @impure
    lazy val lastCommit: Either[String, Commit] = {
        lastCommitSha.map( lastCommitSha => {
            val rootCommit = Commit.root(this)
            if (lastCommitSha == rootCommit.sha) {
                Right(rootCommit)
            } else {
                IO.read(this, commitsFolder/lastCommitSha, Commit.deserialize)
            }
        }).flatten
    }

    @impure
    lazy val lastCommitedIndex: Either[String, CommitedIndex] = {
        for {
            lastCommit <- lastCommit
            lastCommitedIndex <- lastCommit.index
        } yield lastCommitedIndex
    }

    @impure
    lazy val hasUncommitedChanges: Either[String, Boolean] = {
        val tuple = for {
            notCommitedCurrentIndex <- notCommitedCurrentIndex
            lastCommitedIndex <- lastCommitedIndex
        } yield (notCommitedCurrentIndex, lastCommitedIndex)

        val modifiedIndex = listPotentiallyModifiedFilesAsIndex()
        val deletedIndex = listPotentiallyDeletedFilesAsIndex()

        tuple.map( t => {
            val uncommitedCurrentIndex = t._1
            val lastCommitedIndex = t._2

            uncommitedCurrentIndex.newfiles(lastCommitedIndex).nonEmpty ||
            uncommitedCurrentIndex.modified(lastCommitedIndex).nonEmpty ||
            lastCommitedIndex.deleted(uncommitedCurrentIndex).nonEmpty ||
            uncommitedCurrentIndex.modified(modifiedIndex).nonEmpty ||
            uncommitedCurrentIndex.deleted(deletedIndex).nonEmpty
        })
    }

    @impure
    def init(): Either[String, String] = {
        try {
            repositoryFolder.createDirectories()

            indexFile.createFile()

            val rootCommit = Commit.root(this)
            val masterBranch = Branch(this, "master", rootCommit.sha)
            headFile.write(masterBranch.name)

            branchesFolder.createDirectories()
            IO.write(masterBranch)

            tagsFolder.createDirectories()

            blobsFolder.createDirectories()
            commitsFolder.createDirectories()
            indexesFolder.createDirectories()
            IO.write(CommitedIndex.empty(this))

            Right("Repository initialized.")
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
    }

    def listAllFiles(): List[File] = {
        workingDirectory.listRecursively
            .filter(file => {
                !(
                    file == repositoryFolder ||
                    file.isDirectory ||
                    file.isChildOf(repositoryFolder)
                )
            })
            .toList
    }

    def listPotentiallyModifiedFilesAsIndex(): NotCommitedIndex = {
        val potentiallyModifiedFiles = listAllFiles()
            .filter(file => file.exists)
            .map(file => (relativize(file), Util.shaFile(file)))
            .toMap

        NotCommitedIndex(this, potentiallyModifiedFiles)
    }

    def listPotentiallyDeletedFilesAsIndex(): NotCommitedIndex = {
        val potentiallyDeletedFiles = listAllFiles()
            .map(file => (relativize(file), ""))
            .toMap

        NotCommitedIndex(this, potentiallyDeletedFiles)
    }

    def listPotentiallyUntrackedFiles(): List[String] = {
        listAllFiles()
            .map(file => relativize(file))
    }

    def relativize(file: File): String = {
        workingDirectory.relativize(file).toString
    }
}

object Repository {

    // TODO: change to sgit
    val directoryName: String = "vcs"
    val headPath: String = "head"
    val indexPath: String = "index"
    val detachedPath: String = "detached"
    val commitsPath: String = "commits"
    val indexesPath: String = "indexes"
    val branchesPath: String = "branches"
    val tagsPath: String = "tags"
    val blobsPath: String = "blobs"

    @impure
    @tailrec
    final def findRepository(folder: File): Option[File] = {
        val repoFile = folder/Repository.directoryName
        if (repoFile.isDirectory()) {
            Some(repoFile)
        } else {
            val parent = folder.parent
            if (parent == null) return None
            findRepository(parent)
        }
    }
}
