package com.sardois.sgit

import java.io.IOException
import java.nio.file.NoSuchFileException

import better.files.File


object Main {

    @impure
    def call(args: Config, func: (File, File, Config) => Option[String]): Unit = {
        try {
            Repository.callInside(args, func) match {
                case Some(value) => print(value)
                case None =>
            }
        } catch {
            case ex: NoSuchFileException => error("File not found: " + ex.getMessage)
            case ex: Throwable => error(ex.getMessage)
        }
    }

    @impure
    def print(str: String): Unit = {
        println(str)
    }

    @impure
    def error(str: String): Unit = {
        println(str)
    }

    def main(args : Array[String]): Unit = {
        Parser().parse(args, Config()) match {
            case Some(config) => config.mode match {

                case "init" =>
                    val currentFolder = Repository.currentFolder
                    IORepository.init(currentFolder) match {
                        case Left(value) => error(value)
                        case Right(value) =>
                    }

                case "add" => call(config, IOIndex.add)
                case "commit" => call(config, IOCommit.commit)
                case "status" => call(config, IOIndex.status)
                case "branch" => {
                    config.list match {
                        case true => call(config, IOCheckable.list)
                        case false => call(config, IOCheckable.create)
                    }
                }
                case "tag" => call(config, IOCheckable.create)
                case "checkout" => call(config, IOCommit.checkout)

                case _ =>
            }
            case _ =>
        }
    }
}
