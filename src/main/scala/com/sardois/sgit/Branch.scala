package com.sardois.sgit

import better.files.File

class Branch(val name: String, val commitSha: String) extends Checkable {

    def getType(): String = {
        Branch.getType
    }

    def getPath(): String = {
        Repository.branchesPath
    }
}

object Branch {

    def apply(name: String, commitSha: String): Branch = {
        new Branch(name, commitSha)
    }

    def getType: String = {
        "branch"
    }

    def master: Branch = {
        Branch("master", Commit.root.sha())
    }
}

object IOBranch {

    def getBranchesFolder(repoFolder: File): File = {
        repoFolder/Repository.branchesPath
    }
}
