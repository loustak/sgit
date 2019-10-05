package com.lucassardois

import better.files._
import scala.annotation.implicitNotFound

/* Used to get a path to the repository in real conditions */
object RepositoryReal {

    def getName(): String = ".sgit"

    def getCurrentFolder(): String = {
        System.getProperty("user.dir")
    }

    def getRepositoryPath(): String = {
        getCurrentFolder() + "/" + getName()
    }
}

object Repository {

    /* Return wether or not a sgit repository
        exists at the given path */
    def existsAt(path: String): Boolean = {
        val file = File(path)
        return file.isDirectory()
    }
}

class Repository(
    val path: String,
    val head: Branch,
    val branches: List[Branch]
    ) {

    def getBranchesPath(): String = {
        path + "/" + Branch.getFilePath()
    }

    def branchesFileExists(): Boolean = {
        val branchesPath = getBranchesPath()
        val file = File(branchesPath)
        file.exists()
    }
}

object IORepository {

    @impure
    def init(path: String): Either[String, Repository] = {
        if (Repository.existsAt(path)) {
            return Left("This is already an sgit repository.")
        }

        val master = new Branch("master", NoParentCommit)
        val branches = List(master)

        val repo = new Repository(path, master, branches)

        write(repo)
        Right(repo)
    }

    /* Try to load the repository */
    @impure
    def initFromFolder(path: String): Either[String, Repository] = {
        if (!Repository.existsAt(path)) {
            return Left("Abort, not a sgit repository.")
        }
        return Left("lol")
    }

    /* Delete all the files in a repository, this function
    is not available for the user but only for tests.
    This function is unpure since it only manage files. */
    @impure
    def delete(repo: Repository) = {
        val file = File(repo.path)
        file.delete()
    }

    /* Unpure function wich write the repository to the disk. */
    @impure
    def write(repo: Repository) = {
        val file = File(repo.path)
        file.createDirectory()

        IOBranches.write(repo)
    }
}