package com.sardois.sgit
import better.files.File

case class Commit(repository: Repository, message: String, indexSha: String, parentCommitSha: String, author: String, date: String) extends IO{

    @impure
    lazy val parentCommit: Either[String, Commit] = {
        val commitFile = repository.commitsFolder/parentCommitSha
        IO.read(repository, commitFile, Commit.deserialize)
    }

    override val file: File = repository.commitsFolder/sha

    def serialize: String = {
        val list = List(message, indexSha, parentCommitSha, author, date)
        list.mkString(System.lineSeparator())
    }

    def sha: String = {
        Util.shaString(serialize)
    }
}

object Commit {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, Commit] = {
        val lines = str.linesIterator.toList
        val message = lines(1)
        val indexSha = lines(2)
        val parentCommitSha = lines(3)
        val author = lines(4)
        val date = lines(5)

        Right(Commit(repository, message, indexSha, parentCommitSha, author, date))
    }

    def rootSha: String = {
        Util.shaString("root commit")
    }
}
