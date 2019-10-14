package com.sardois.sgit

import java.io.IOException
import java.security.MessageDigest

import better.files._

import scala.annotation.tailrec

object Util {

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

    def pathsToFiles(paths: List[String]): List[File] = {
        paths.map( path => File(path))
    }

    def filesToPath(files: List[File]): List[String] = {
        files.map( file => file.pathAsString)
    }

    def getNestedFiles(files: List[File]): List[File] = {
        files.flatMap( file => {
            if (file.isDirectory) file.listRecursively
            else List(file)
        })
    }

    def removeDirectories(files: List[File]): List[File] = {
        files.filter( file => !file.isDirectory)
    }

    def pathsToUsableFiles(paths: List[String]): List[File] = {
        val files = pathsToFiles(paths)
        val nestedFiles = getNestedFiles(files)
        removeDirectories(nestedFiles)
    }

    def formatList(list: List[String], symbol: String): String = {
        list.map( elem => {
            symbol + elem.toString
        }).mkString(System.lineSeparator())
    }
}

