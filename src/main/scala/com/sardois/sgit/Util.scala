package com.sardois.sgit

import java.security.MessageDigest

import better.files._

object Util {

    def shaFile(file: File): String = {
        file.sha256.toLowerCase
    }

    def shaString(str: String): String = {
        MessageDigest.getInstance("SHA-256")
            .digest(str.getBytes("UTF-8"))
            .map("%02x".format(_)).mkString
    }
}

