package com.sardois.sgit

import java.io.IOException
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

    def pathsToFiles(paths: List[String]): List[File] = {
        paths.map( path => File(path))
    }

    def handleIOException(func: () => Option[String]): Option[String] = {
        try {
            func()
        } catch {
            case ex: IOException => Some(ex.getMessage)
        }
    }

    def optionToEither[A](o: Option[A]): Either[A, Unit] = {
        o match {
            case Some(value) => Left(value)
            case None => Right(None)
        }
    }

    def eitherToOption[B](e: Either[String, B]): Option[String] = {
        e match {
            case Left(value) => Some(value)
            case Right(value) => None
        }
    }
}

