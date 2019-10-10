package com.sardois.sgit

import better.files._

class Branch(val name: String, val commit: Commit) {

}

object IOBranch {

    def read(file: File): Either[String, Branch] = {
        val branchName = file.name
        val lines = file.lines().toArray
        if (!lines.isDefinedAt(0)) {
            return Left("Branch " + branchName + " commit hash is invalid.")
        }
        val commit = new Commit()
        Right(new Branch(branchName, commit))
    }

    @impure
    def write(file: File, branch: Branch): Unit = {
        val fileBranch = file/branch.name
        fileBranch.write(branch.commit.toString())
    }

    @impure
    def writeAll(file: File, branches: List[Branch]): Unit = {
        branches.foreach( (branch) => {
            write(file, branch)
        })
    }
}