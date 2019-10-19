package com.sardois.sgit
import better.files.File

case class Branch(repository: Repository, name: String, commitSha: String) extends IO {

    @impure
    lazy val commit: Either[String, Commit] = {
        val commitFile = repository.commitsFolder/commitSha
        IO.read(repository, commitFile, Commit.deserialize)
    }

    override val file: File = repository.branchesFolder/name

    override def serialize: String = commitSha

    def moveToCommit(newCommitSha: String): Branch = {
        Branch(repository, name, newCommitSha)
    }
}

object Branch {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, Branch] = {
        Right(Branch(repository, fileName, str))
    }
}

case class Tag(repository: Repository, name: String, commitSha: String) extends IO {

    @impure
    lazy val commit: Either[String, Commit] = {
        val commitFile = repository.tagsFolder/commitSha
        IO.read(repository, commitFile, Commit.deserialize)
    }

    override val file: File = repository.tagsFolder/name

    override def serialize: String = commitSha

    def moveToCommit(newCommitSha: String): Branch = {
        Branch(repository, name, newCommitSha)
    }
}

object Tag {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, Tag] = {
        Right(Tag(repository, fileName, str))
    }
}
