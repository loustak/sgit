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
            case ex: IOException => Left(ex.toString)
        }
    }

    @impure
    def read[A <: IO](repository: Repository, file: File, deserialize: (Repository, String, String) => Either[String, A]): Either[String, A] = {
        handleIOException[A]( () => {
            val str = file.contentAsString
            deserialize(repository, file.name, str)
        })
    }

    @impure
    def readAll[A <: IO](repository: Repository, folder: File, deserialize: (Repository, String, String) => Either[String, A]): Either[String, List[A]] = {
        handleIOException( () => {
            val files = folder.list
            val t = files.map( file => deserialize(repository, file.name, file.contentAsString)).toList

            val (lefts, rights) = t.partitionMap(identity)
            if (!lefts.isEmpty) return Left(lefts(0))
            Right(rights)
        })
    }

    @impure
    def write(io: IO): Either[String, String] = {
        handleIOException( () => {
            io.file.write(io.serialize)
            Right("")
        })
    }

    @impure
    def writeAll(list: List[IO]): Either[String, String] = {
        handleIOException( () => {
            list.foreach( io => io.file.write(io.serialize))
            Right("")
        })
    }
}
