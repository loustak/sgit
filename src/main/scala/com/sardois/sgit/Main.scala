package com.sardois.sgit

object Main {

    @impure
    def init(): Unit = {
        val currentFolder = Util.currentFolder
        Repository.findRepository(currentFolder) match {
            case Some(value) => error("This folder is already a " + Repository.directoryName + " repository.")
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
    def call(config: Config, command: (Repository, Config) => Either[String, String]): Unit = {
        val currentFolder = Util.currentFolder
        Repository.findRepository(currentFolder) match {
            case None => error("Not a " + Repository.directoryName + " repository.")
            case Some(repositoryFolder) => {
                val repoFolder = Repository(repositoryFolder)
                command(repoFolder, config) match {
                    case Left(value) => error(value)
                    case Right(value) => print(value)
                }
            }
        }
    }

    @impure
    def print(str: String): Unit = {
        println(str)
    }

    @impure
    def error(str: String): Unit = {
        println("error: " + str)
    }

    def main(args : Array[String]): Unit = {
        Parser().parse(args, Config()) match {
            case Some(config) => config.mode match {

                case "init" => init()
                case "add" => call(config, Command.add)
                case "commit" => call(config, Command.commit)
                case "status" => call(config, Command.status)
                case "branch" => config.list match {
                    case true => call(config, Command.listBranchAndTags)
                    case false =>
                }
                case _ =>
            }
            case _ =>
        }
    }
}
