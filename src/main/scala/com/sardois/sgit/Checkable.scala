package com.sardois.sgit

import better.files.File
import com.sardois.sgit.CheckableEnum.CheckableEnum

class Checkable(val checkableType: CheckableEnum, val name: String, val commitSha: String) {

    override def toString: String = {
        checkableType.toString + " " + name
    }
}

object CheckableEnum extends Enumeration {

    type CheckableEnum = Value

    val BRANCH = Value("branch")
    val TAG = Value("tag")
}

object Checkable {

    def apply(checkableType: CheckableEnum, name: String, commitSha: String): Checkable = {
        new Checkable(checkableType, name, commitSha)
    }
}

object Branch {

    def apply(name: String, commitSha: String): Checkable = {
        Checkable(CheckableEnum.BRANCH, name, commitSha)
    }

    def master: Checkable = {
        Branch("master", Commit.root.sha)
    }
}

object Tag {

    def apply(name: String, commitSha: String): Checkable = {
        Checkable(CheckableEnum.TAG, name, commitSha)
    }
}

object IOCheckable {

    def getBranchesFolder(repoFolder: File): File = {
        repoFolder/Repository.branchesPath
    }


    def getTagsFolder(repoFolder: File): File = {
        repoFolder/Repository.tagsPath
    }

    def getCheckableTypeFolder(repoFolder: File, checkable: Checkable): File = {
        checkable.checkableType match {
            case CheckableEnum.BRANCH => getBranchesFolder(repoFolder)
            case CheckableEnum.TAG => getTagsFolder(repoFolder)
        }
    }

    def getCheckableFile(repoFolder: File, checkable: Checkable): File = {
        val checkableFolder = getCheckableTypeFolder(repoFolder, checkable)
        checkableFolder/checkable.name
    }

    @impure
    def setToSha(checkableFile: File, newSha: String): Unit = {
        checkableFile.clear()
        checkableFile.write(newSha)
    }

    @impure
    def list(checkableFolder: File): Array[File] = {
        checkableFolder.list.toArray
    }

    @impure
    def list(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val branchesFolder = getBranchesFolder(repoFolder)
        val tagsFolder = getTagsFolder(repoFolder)

        val branchesList = list(branchesFolder).map( f => f.name)
        val tagsList = list(tagsFolder).map( f => f.name)

        val newLine = System.lineSeparator()
        val branchesString = branchesList.mkString(newLine)
        val tagsString = tagsList.mkString(newLine)

        val listString = List(
            "Branches:", branchesString,
            "Tags:", tagsString
        )

        Some(listString.mkString(newLine))
    }

    @impure
    def find(repoFolder: File, name: String): (Boolean, Commit) = {
        val commitsFolder = IOCommit.getCommitsFolder(repoFolder)

        val branchesFiles = getBranchesFolder(repoFolder)
        val branchMatch = branchesFiles.list.toList.filter( file => file.name == name)
        if (branchMatch.length == 1) {
            val commitSha = branchMatch(0).contentAsString
            return (false, IOCommit.read(commitsFolder, commitSha))
        }

        val tagsFiles = getTagsFolder(repoFolder)
        val tagMatch = tagsFiles.list.toList.filter( file => file.name == name)
        if (tagMatch.length == 1) {
            val commitSha = tagMatch(0).contentAsString
            return (true, IOCommit.read(commitsFolder, commitSha))
        }

        val commits = commitsFolder.list
        val matchedCommits = commits.filter( file => {
            file.name.startsWith(name)
        }).toList

        if (matchedCommits.length == 1) {
            return (true, IOCommit.read(commitsFolder, matchedCommits(0).name))
        } else if (matchedCommits.length > 1) {
            throw new RuntimeException("Multiple commits found, use a longer commit hash.")
        }

        throw new RuntimeException("No branch, tags or commits found.")
    }

    @impure
    def create(repoFolder: File, checkable: Checkable): Unit = {
        val checkableFile = getCheckableFile(repoFolder, checkable)

        if (checkableFile.exists) {
            throw new RuntimeException("This name is already used")
        }

        checkableFile.write(checkable.commitSha)
    }

    @impure
    def create(repoFolder: File, commandFolder: File, args: Config): Option[String] = {
        val branchName = args.branchName
        val tagName = args.tagName

        val commitSha = IOHead.getPreviousCommit(repoFolder).sha

        if (!branchName.isEmpty) {
            val newBranch = Branch(branchName, commitSha)
            create(repoFolder, newBranch)
        } else if (!tagName.isEmpty) {
            val newTag = Tag(tagName, commitSha)
            create(repoFolder, newTag)
        }

        None
    }
}