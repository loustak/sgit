package com.lucassardois

import better.files._

object Parser {
    object Empty { }
    object NotACommand { }

    def parse(args: Array[String]) = {

        val argsList = args.toList

        argsList match {
            case Nil => Empty
            case "" :: Nil => Empty
            case "init" :: x => {
                val file = File(Repository.getRepositoryPath())
                IORepository.init(file)
            }
            case _ => NotACommand
        }
    }
}