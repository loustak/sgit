package com.sardois.sgit

import better.files.File

case class CurrentIndex(file: File) extends IO {

    override def serialize: String = {
        ""
    }

    @impure
    def init(): Unit = {
        file.createDirectories()
    }
}
