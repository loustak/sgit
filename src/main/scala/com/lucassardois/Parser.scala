package com.lucassardois

object Parser {
    object Empty { }
    object NotACommand { }

    def parse(args: Array[String]) = {

        val argsList = args.toList

        argsList match {
            case Nil => Empty
            case "" :: Nil => Empty
            case "init" :: x => {
                val path = RepositoryReal.getRepositoryPath()
                RepositoryConstructor(path)
            }
            case _ => NotACommand
        }
    }
}