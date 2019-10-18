package com.sardois.sgit

import better.files._

case class Head(file: File) extends IO {

    lazy val branch: Branch = ???

    override def serialize: String = {
        branch.name
    }
}

object Head {

    def deserialize(str: String): Either[String, Head] = {
        ???
    }
}
