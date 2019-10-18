package com.sardois.sgit

import java.io.IOException

import better.files._

import scala.annotation.tailrec

case class Repository(repositoryFolder: File) {

    val workingDirectory: File = repositoryFolder.parent

    val headFile: File = repositoryFolder/Repository.headPath
    val indexFile: File = repositoryFolder/Repository.indexPath
    val branchesFolder: File = repositoryFolder/Repository.branchesPath

    @impure
    lazy val head: Either[String, Head] = {
        IO.read(headFile, Head.deserialize)
    }

    lazy val branches: Either[String, List[Branch]] = {
        IO.readAll(branchesFolder, Branch.deserialize)
    }

    @impure
    def init(): Either[String, String] = {
        try {
            repositoryFolder.createDirectories()

            val masterBranch = Branch("master", Commit.root)
            headFile.write(masterBranch.serialize)

            indexFile.createFile()

            Right("Repository initialized.")
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
    }
}

object Repository {

    // TODO: change to sgit
    val directoryName: String = "vcs"
    val detachedPath: String = "detached"
    val headPath: String = "head"
    val indexPath: String = "index"
    val indexesPath: String = "indexes"
    val branchesPath: String = "branches"
    val tagsPath: String = "tags"
    val blobsPath: String = "blobs"
    val commitsPath: String = "commits"

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
