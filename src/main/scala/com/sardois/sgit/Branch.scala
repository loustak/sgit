package com.sardois.sgit
import better.files.File

case class Branch(name: String, commit: Commit) extends IO {

    override val file: File = ???

    override def serialize: String = ???
}

object Branch {

    def deserialize(str: String): Either[String, Branch] = {
        ???
    }
}
