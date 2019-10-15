package com.sardois.sgit

import better.files._

import scala.annotation.tailrec

object Blob {

    @tailrec
    def shaExists(files: Iterable[File], sha: String): Boolean = {
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

    def getBlobsFolder(repoFolder: File): File = {
        repoFolder/Repository.getBlobsPath()
    }

    @impure
    def write(blobsFolder: File, file: File): Unit = {
        val sha = Util.shaFile(file)
        val files = blobsFolder.list.toList

        if (!Blob.shaExists(files, sha)) {
            // The file doesn't exists, write it
            val content = file.contentAsString
            val sha = Util.shaString(content)
            val newBlob = blobsFolder/sha
            newBlob.write(content)
        }
    }

    @tailrec
    @impure
    def writeAll(blobsFolder: File, files: Iterable[File]): Unit = {
        if (files == Nil) return
        val file = files.head
        write(blobsFolder, file)
        writeAll(blobsFolder, files.tail)
    }
}