package com.sardois.sgit

import java.io.IOException
import java.security.MessageDigest

import better.files._

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

    def handleException(func: () => Unit): Option[String] = {
        try {
            func()
            None
        } catch {
            case ex: IOException => Some(ex.getMessage)
            case ex: IllegalArgumentException => Some(ex.getMessage)
            case ex: RuntimeException => Some(ex.getMessage)
        }
    }
}

