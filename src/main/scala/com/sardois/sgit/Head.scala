package com.sardois.sgit

import better.files._
import com.sardois.sgit.CheckableEnum.CheckableEnum

case class Head(
                   repository: Repository,
                   checkableType: CheckableEnum,
                   branchTagOrCommitSha: String
               ) extends IO {

    val isBranch: Boolean = checkableType == CheckableEnum.BRANCH
    val isTag: Boolean = checkableType == CheckableEnum.TAG
    val isCommit: Boolean = checkableType == CheckableEnum.COMMIT

    val isDetached: Boolean = isTag || isCommit

    @impure
    lazy val pointedBranch: Either[String, Branch] = {
        if (isBranch) {
            val branchFile = repository.branchesFolder / branchTagOrCommitSha
            IO.read(repository, branchFile, Branch.deserialize)
        } else {
            Left("The head is not pointing to a branch but to a " + checkableType.toString + ".")
        }
    }

    @impure
    lazy val pointedTag: Either[String, Tag] = {
        if (isTag) {
            val tagFile = repository.tagsFolder / branchTagOrCommitSha
            IO.read(repository, tagFile, Tag.deserialize)
        } else {
            Left("The head is not pointing to a tag but to a " + checkableType.toString + ".")
        }
    }

    @impure
    lazy val pointedCommit: Either[String, Commit] = {
        if (isCommit) {
            val commitFile = repository.commitsFolder / branchTagOrCommitSha
            IO.read(repository, commitFile, Commit.deserialize)
        } else {
            Left("The head is not pointing to a commit but to a " + checkableType.toString + ".")
        }
    }

    @impure
    lazy val commit: Either[String, Commit] = {
        if (isBranch) pointedBranch.map(branch => branch.commit).flatten
        else if (isTag) pointedTag.map(tag => tag.commit).flatten
        else pointedCommit
    }

    val file: File = repository.headFile

    override def serialize: String = {
        checkableType.toString + " " + branchTagOrCommitSha
    }
}

object CheckableEnum extends Enumeration {

    type CheckableEnum = Value

    val BRANCH = Value("branch")
    val TAG = Value("tag")
    val COMMIT = Value("commit")
}

object Head {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, Head] = {
        val split = str.split(" ")
        if (split.length != 2) return Left("Head file has invalid format.")

        val checkableType = split(0)
        val branchTagOrCommit = split(1)

        if (checkableType == CheckableEnum.BRANCH.toString) {
            Right(Head(repository, CheckableEnum.BRANCH, branchTagOrCommit))
        } else if (checkableType == CheckableEnum.TAG.toString) {
            Right(Head(repository, CheckableEnum.TAG, branchTagOrCommit))
        } else if (checkableType == CheckableEnum.COMMIT.toString) {
            Right(Head(repository, CheckableEnum.COMMIT, branchTagOrCommit))
        } else Left("The head type is invalid.")
    }
}
