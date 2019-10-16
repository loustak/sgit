package com.sardois.sgit

import better.files.File
import com.sardois.sgit.CheckableType.CheckableType

class Checkable(val checkableType: CheckableType, val name: String, val commitSha: String) {

    override def toString: String = {
        checkableType.toString + " " + name
    }
}

object CheckableType extends Enumeration {

    type CheckableType = Value

    val BRANCH = Value("branch")
    val TAG = Value("tag")
}

object Checkable {

    def apply(checkableType: CheckableType, name: String, commitSha: String): Checkable = {
        new Checkable(checkableType, name, commitSha)
    }
}

object Branch {

    def apply(name: String, commitSha: String): Checkable = {
        Checkable(CheckableType.BRANCH, name, commitSha)
    }

    def master: Checkable = {
        Branch("master", Commit.root.sha)
    }
}

object Tag {

    def apply(name: String, commitSha: String): Checkable = {
        Checkable(CheckableType.TAG, name, commitSha)
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
            case CheckableType.BRANCH => getBranchesFolder(repoFolder)
            case CheckableType.TAG => getTagsFolder(repoFolder)
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
    def find(repoFolder: File, name: String): Commit = {
        val commitsFolder = IOCommit.getCommitsFolder(repoFolder)

        val checkableFolders = Iterable(
            getBranchesFolder(repoFolder),
            getTagsFolder(repoFolder)
        )

        checkableFolders.foreach( checkableFolder => {
            val checkables = checkableFolder.list
            val checkableMatched = checkables.filter( file => {
                file.name == name
            }).toList

            if (checkableMatched.length == 1) {
                val commitSha = checkableMatched(0).contentAsString
                return IOCommit.read(commitsFolder, commitSha)
            }
        })

        val commits = commitsFolder.list
        val matchedCommits = commits.filter( file => {
            file.name.contains(name)
        }).toList

        if (matchedCommits.length == 1) {
            return IOCommit.read(commitsFolder, matchedCommits(0).name)
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