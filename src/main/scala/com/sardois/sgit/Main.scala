package com.sardois.sgit

object Main {

    @impure
    def init(): Unit = {
        val currentFolder = Util.currentFolder
        Repository.findRepository(currentFolder) match {
            case Some(value) => println("This folder is already a " + Repository.directoryName + " repository")
            case None => {
                val repositoryFolder = currentFolder/Repository.directoryName
                val repo = Repository(repositoryFolder)
                repo.init().fold(
                    (errorMessage) => error(errorMessage),
                    (successMessage) => print(successMessage)
                )
            }
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

                case "init" => init()
                case _ =>
            }
            case _ =>
        }
    }
}
