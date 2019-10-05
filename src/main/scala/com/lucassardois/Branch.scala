package com.lucassardois

import better.files._
import scala.annotation.tailrec

object Branch {
    
    def getFilePath() = "branches"
}

class Branch(val name: String, val commit: Commit) {

    def isMaster(): Boolean = name == "master"

    override def toString(): String = {
        name + " " + commit.toString()
    }
}

object IOBranch {

    /* Unpure function wich write all the branches of the repo. */
    @impure
    def writeBranches(repo: Repository): Unit = {
        val branchPath = repo.getBranchesPath()
        val file = File(branchPath) 
        
        file.createFileIfNotExists()
        file.clear()

        write(file, repo.branches)
    }

    /* Unpure function wich write one branch at the time
    to the repo's branches file. */
    @impure @tailrec
    final def write(file: File, branches: List[Branch]): Unit = {
        if (branches != Nil) {
            file.appendLine(branches.head.toString())
            write(file, branches.tail)
        }
    }
}