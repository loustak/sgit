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
            branch <- head.pointedBranch
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
    lazy val commitsBranch: Either[String, List[Commit]] = {
        @tailrec
        def rec(commitSha: String, list: List[Commit]): Either[String, List[Commit]] = {
            if (commitSha == Commit.root(this).sha) return Right(list)
            val commitFile = commitsFolder/commitSha
            IO.read(this, commitFile, Commit.deserialize) match {
                case Left(error) => Left(error)
                case Right(commit) => {
                    rec(commit.parentCommitSha, commit :: list)
                }
            }
        }

        lastCommit.map(lastCommit => {
            rec(lastCommit.sha, Nil)
        }).flatten
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
    lazy val lastCommit: Either[String, Commit] = {
        for {
            head <- head
            lastCommit <- head.commit
        } yield lastCommit
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
        IO.handleIOException( () => {
            repositoryFolder.createDirectories()

            indexFile.createFile()

            val rootCommit = Commit.root(this)
            val masterBranch = Branch(this, "master", rootCommit.sha)
            val head = Head(this, CheckableEnum.BRANCH, masterBranch.name)

            branchesFolder.createDirectories()

            tagsFolder.createDirectories()

            blobsFolder.createDirectories()
            commitsFolder.createDirectories()
            indexesFolder.createDirectories()

            for {
                _ <- IO.write(head)
                _ <- IO.write(masterBranch)
                result <- IO.write(CommitedIndex.empty(this))
            } yield result
        })
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

    val directoryName: String = ".sgit"
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
