package com.lucassardois

import better.files._
import scala.annotation.tailrec

object Repository {

    def getDirectoryName(): String = ".sgit"

    def getCurrentFolder(): String = {
        System.getProperty("user.dir")
    }

    def getRepositoryPath(): String = {
        getCurrentFolder() + "/" + getDirectoryName()
    }

    /* Return wether or not a sgit repository
        exists at the given path if not, check for parent */
    @tailrec
    def isARepository(file: File): Boolean = {
        val repoFile = file/getDirectoryName()
        if (repoFile.isDirectory()) {
            return true
        } else {
            val parent = file.parent
            if (parent == null) return false
            isARepository(parent)
        }
    }
}

class Repository(
    val file: File,
    val head: Branch,
    val branches: List[Branch]
    ) {

    def getBranchFile(): File = {
        file/Branch.getFilePath()
    }

    def branchesFileExists(): Boolean = {
        getBranchFile().exists()
    }
}

object IORepository {

    @impure
    def init(file: File): Either[String, Repository] = {
        if (Repository.isARepository(file)) {
            return Left("This is already an sgit repository.")
        }

        val master = new Branch("master", NoParentCommit)
        val branches = List(master)

        val repo = new Repository(file, master, branches)

        IORepository.write(repo)
        Right(repo)
    }

    /* Try to load the repository */
    @impure
    def initFromFolder(file: File): Either[String, Repository] = {
        if (!Repository.isARepository(file)) {
            return Left("Abort, not a sgit repository.")
        }
        return Left("lol")
    }

    /* Unpure function wich write the repository to the disk. */
    @impure
    def write(repo: Repository) = {
        repo.file.createDirectories()
        IOBranches.write(repo)
    }
}