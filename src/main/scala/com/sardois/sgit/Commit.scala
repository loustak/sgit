package com.sardois.sgit
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import better.files.File

case class Commit(
     repository: Repository,
     message: String,
     indexSha: String,
     parentCommitSha: String,
     author: String = "Anonymous",
     dateString: String = Commit.dateFormatter.format(Calendar.getInstance().getTime)
         ) extends IO {

    @impure
    lazy val index: Either[String, CommitedIndex] = {
        val indexFile = repository.indexesFolder/indexSha
        IO.read(repository, indexFile, CommitedIndex.deserialize)
    }

    @impure
    lazy val parentCommit: Either[String, Commit] = {
        val commitFile = repository.commitsFolder/parentCommitSha
        IO.read(repository, commitFile, Commit.deserialize)
    }

    lazy val date: Date = Commit.dateFormatter.parse(dateString)

    override val file: File = repository.commitsFolder/sha

    def serialize: String = {
        val list = List(message, indexSha, parentCommitSha, author, dateString)
        list.mkString(System.lineSeparator())
    }

    def sha: String = {
        Util.shaString(serialize)
    }
}

object Commit {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, Commit] = {
        val lines = str.linesIterator.toList
        val message = lines(0)
        val indexSha = lines(1)
        val parentCommitSha = lines(2)
        val author = lines(3)
        val date = lines(4)

        Right(Commit(repository, message, indexSha, parentCommitSha, author, date))
    }

    def root(repository: Repository): Commit = {
        Commit(repository, "root commit", CommitedIndex.empty(repository).sha, "", "", "")
    }

    def dateFormatter = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
}
