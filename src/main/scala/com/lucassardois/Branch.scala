package com.lucassardois

import better.files._
import scala.annotation.tailrec

object Branch {
    
    def getFilePath(): String = "branches"

    def createFromLine(line: String): Either[String, Branch] = {
        val split = line.split(" ")

        if (!split.isDefinedAt(0)) {
            return Left("Branch has invalid name.")
        }
        if (!split.isDefinedAt(1)) {
            return Left("Branch has invalid commit.")
        }

        val name = split(0)
        val commit = NoParentCommit

        Right(new Branch(name, commit))
    }
}

class Branch(val name: String, val commit: Commit) {

    def isMaster(): Boolean = name == "master"

    override def toString(): String = {
        name + " " + commit.toString()
    }
}

/* Manipulate a single branch */
object IOBranch {

    @impure
    def write(file: File, branch: Branch): Unit = {
        file.appendLine(branch.toString())
    }
}

/* Allow to manipulate a list of branch */
object IOBranches {

    /* Return either an error, or the list of all the branch reads */
    def read(repo: Repository): Either[String, List[Branch]] = {
        if (!repo.branchesFileExists()) {
            return Left("Abort, no branches found: invalid repository.")
        }

        val file = repo.getBranchFile()
        val lines = file.lines()

        val either = recRead(lines.toList)

        val (errors, branches) = either.partitionMap(identity)
        if (errors.isEmpty) {
            Right(branches)
        } else {
            Left(errors(0))
        }
    }

    def recRead(lines: List[String]): List[Either[String, Branch]] = {
        if (lines == Nil) {
            return Nil
        }

        val either = Branch.createFromLine(lines.head)

        either match {
            case Left(x) => Left(x) :: recRead(lines.tail)
            case Right(y) => Right(y) :: recRead(lines.tail)
        }
    }

    /* Unpure function wich write all the branches of the repo. */
    @impure
    def write(repo: Repository): Unit = {
        val file = repo.getBranchFile()
        
        file.createFileIfNotExists()
        file.clear()

        recWrite(file, repo.branches)
    }

    /* Unpure function wich write one branch at the time
    to the repo's branches file. */
    @impure @tailrec
    final def recWrite(file: File, branches: List[Branch]): Unit = {
        if (branches != Nil) {
            IOBranch.write(file, branches.head)
            recWrite(file, branches.tail)
        }
    }
}