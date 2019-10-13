package com.sardois.sgit

import java.io.IOException
import java.nio.file.NoSuchFileException

import better.files.File


object Main {

    def call(args: Config, func: (File, File, Config) => Unit): Unit = {
        try {
            Repository.callInside(args, func)
        } catch {
            case ex: NoSuchFileException => error("File not found: " + ex.getMessage)
            case ex: Exception => error(ex.toString)
        }
    }

    def error(str: String): Unit = {
        println(str)
    }

    def main(args : Array[String]): Unit = {
        Parser().parse(args, Config()) match {
            case Some(config) => config.mode match {

                case "init" => {
                    val currentFolder = Repository.getCurrentFolder()
                    IORepository.init(currentFolder) match {
                        case Left(value) => error(value)
                        case Right(value) =>
                    }
                }

                case "add" => {
                    call(config, IOIndex.add)
                }

                case "rm" => {
                    call(config, IOIndex.remove)
                }

                case "status" => {
                }

                case _ =>
            }
            case _ =>
        }
    }
}
