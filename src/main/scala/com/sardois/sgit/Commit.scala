package com.sardois.sgit

import java.io.IOException

import better.files.File

object NoParentCommit extends Commit {

}

class Commit() {

    override def toString(): String = "hjhfkjhfdsjdsf"
}

object Commit {

}

object IOCommit {

    def getCommitsFile(repoFolder: File): Either[String, File] = {
        try {
            val commitsFile = repoFolder/Repository.getCommitsPath()
            Right(commitsFile)
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
    }

    def write(commitsFolder: File, commit: Commit): Option[String] = {
        ???
    }


}