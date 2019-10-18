package com.sardois.sgit

import java.security.MessageDigest

import better.files._

object Util {

    def currentPath: String = {
        System.getProperty("user.dir")
    }

    def currentFolder: File = {
        File(currentPath)
    }

    def shaFile(file: File): String = {
        shaString(file.contentAsString)
    }

    /* Change this hash function to change the hash
     * algorithm used by sgit globally.
     */
    def shaString(str: String): String = {
        MessageDigest.getInstance("SHA-256")
            .digest(str.getBytes("UTF-8"))
            .map("%02x".format(_)).mkString
    }

    def pathsToFiles(paths: Iterable[String]): Iterable[File] = {
        paths.map( path => File(path))
    }

    def filesToPath(files: Iterable[File]): Iterable[String] = {
        files.map( file => file.pathAsString)
    }

    def filesToPath(files: File *): Array[String] = {
        files.map( file => file.pathAsString).toArray
    }

    def getNestedFiles(files: Iterable[File]): Iterable[File] = {
        files.flatMap( file => {
            if (file.isDirectory) file.listRecursively
            else List(file)
        })
    }

    def removeDirectories(files: Iterable[File]): Iterable[File] = {
        files.filter( file => !file.isDirectory)
    }

    def pathsToUsableFiles(paths: Iterable[String]): Iterable[File] = {
        val files = pathsToFiles(paths)
        val nestedFiles = getNestedFiles(files)
        removeDirectories(nestedFiles)
    }

    def formatList(list: Iterable[String], symbol: String): String = {
        list.map( elem => {
            symbol + elem.toString
        }).mkString(System.lineSeparator())
    }
}

