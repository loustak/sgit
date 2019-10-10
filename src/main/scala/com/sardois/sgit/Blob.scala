package com.sardois.sgit

import java.io.IOException

import better.files._

import scala.annotation.tailrec

object Blob {

    @tailrec
    def shaExists(files: List[File], sha: String): Boolean = {
        if (files == Nil) false
        else {
            val file = files.head
            val shaFile = Util.shaFile(file)
            if (shaFile == sha) true
            else shaExists(files.tail, sha)
        }
    }
}

object IOBlob {

    def getBlobsFolder(repoFolder: File): Either[String, File] = {
        try {
            val blobsFolder = repoFolder/Repository.getBlobsPath()
            Right(blobsFolder)
        } catch {
            case ex: IOException => Left(ex.getMessage)
        }
    }

    @impure
    def write(blobsFolder: File, file: File): Option[String] = {
        try {
            val sha = Util.shaFile(file)
            val files = blobsFolder.list.toList
            if (!Blob.shaExists(files, sha)) {
                // The file doesn't exists, write it
                val content = file.contentAsString
                val sha = Util.shaString(content)
                val newBlob = blobsFolder/sha
                newBlob.write(content)
            }
            None
        } catch {
            case ex: IOException => Some(ex.getMessage)
        }
    }

    @tailrec
    @impure
    def writeAll(blobsFolder: File, files: List[File]): Option[String] = {
        if (files == Nil) None
        else {
            val file = files.head
            write(blobsFolder, file)
            writeAll(blobsFolder, files.tail)
        }
    }
}