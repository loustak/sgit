package com.sardois.sgit

import better.files.File

case class Blob(repository: Repository, sha: String, content: String) extends IO {

    override val file: File = repository.blobsFolder/sha

    @impure
    override def serialize: String = content
}

object Blob {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, Blob] = {
        Right(Blob(repository, fileName, str))
    }
}


