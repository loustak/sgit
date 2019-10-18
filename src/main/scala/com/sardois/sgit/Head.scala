package com.sardois.sgit

import better.files._

case class Head(repository: Repository, branchName: String) extends IO {

    @impure
    lazy val branch: Either[String, Branch] = {
        val branchFile = repository.branchesFolder/branchName
        IO.read(repository, branchFile, Branch.deserialize)
    }

    val file: File = repository.headFile

    override def serialize: String = branchName
}

object Head {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, Head] = {
        val branchName = str
        Right(Head(repository, branchName))
    }
}
