package com.sardois.sgit

import java.io.IOException

import better.files.File

trait IO {

    val file: File

    def serialize: String
}

object IO {

    def handleIOException[A]( func: () => Either[String, A]): Either[String, A] = {
        try {
            func()
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
    }

    @impure
    def read[A <: IO](file: File, deserialize: (String) => Either[String, A]): Either[String, A] = {
        handleIOException[A]( () => {
            val str = file.contentAsString
            deserialize(str)
        })
    }

    @impure
    def readAll[A <: IO](folder: File, deserialize: (String) => Either[String, List[A]]): Either[String, List[A]] = {
        handleIOException( () => {
            val files = folder.list
            files.map( file => deserialize(file.contentAsString))
        })
    }

    @impure
    def write(io: IO): Either[String, String] = {
        handleIOException( () => {
            io.file.write(io.serialize)
            Right("")
        })
    }
}
