package com.sardois.sgit

import java.io.IOException

import better.files._

import scala.annotation.tailrec

case class Repository(repositoryFolder: File) {

    val workingDirectory: File = repositoryFolder.parent

    val headFile: File = repositoryFolder/Repository.headPath
    val indexFile: File = repositoryFolder/Repository.indexPath
    val branchesFolder: File = repositoryFolder/Repository.branchesPath
    val blobsFolder: File = repositoryFolder/Repository.blobsPath
    val commitsFolder: File = repositoryFolder/Repository.commitsPath
    val indexesFolder: File = repositoryFolder/Repository.indexesPath

    @impure
    lazy val head: Either[String, Head] = {
        IO.read(this, headFile, Head.deserialize)
    }

    @impure
    lazy val currentIndex: Either[String, NotStagedIndex] = {
        IO.read(this, indexFile, NotStagedIndex.deserialize)
    }

    @impure
    lazy val branches: Either[String, List[Branch]] = {
        IO.readAll(this, branchesFolder, Branch.deserialize)
    }

    @impure
    lazy val blobs: Either[String, List[Blob]] = {
        IO.readAll(this, blobsFolder, Blob.deserialize)
    }

    @impure
    lazy val indexes: Either[String, List[StagedIndex]] = {
        IO.readAll(this, indexesFolder, StagedIndex.deserialize)
    }

    @impure
    lazy val lastCommitSha: Either[String, String] = {
        for {
            head <- head
            branch <- head.branch
        } yield branch.commitSha
    }

    @impure
    lazy val lastCommit: Either[String, Commit] = {
        for {
            lastCommitSha <- lastCommitSha
            lastCommit <- IO.read(this, commitsFolder/lastCommitSha, Commit.deserialize)
        } yield lastCommit
    }

    @impure
    def init(): Either[String, String] = {
        try {
            repositoryFolder.createDirectories()

            val masterBranch = Branch(this, "master", Commit.rootSha)
            headFile.write(masterBranch.name)

            branchesFolder.createDirectories()
            IO.write(masterBranch)

            indexFile.createFile()

            blobsFolder.createDirectories()
            commitsFolder.createDirectories()
            indexesFolder.createDirectories()

            Right("Repository initialized.")
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
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
