package com.sardois.sgit

import java.security.MessageDigest

import better.files._

object Util {

    def currentPath: String = System.getProperty("user.dir")

    def currentFolder: File = File(currentPath)

    @impure
    def shaFile(file: File): String = {
        shaString(file.contentAsString)
    }

    def shaString(str: String): String = {
        MessageDigest.getInstance("SHA-256")
            .digest(str.getBytes("UTF-8"))
            .map("%02x".format(_)).mkString
    }
}

