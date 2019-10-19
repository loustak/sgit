package com.sardois.sgit

object Main {

    @impure
    def init(): Unit = {
        val currentFolder = Util.currentFolder
        Repository.findRepository(currentFolder) match {
            case Some(value) => {
                UI.printError("This folder is already a " + Repository.directoryName + " repository.")
            }
            case None => {
                val repositoryFolder = currentFolder/Repository.directoryName
                val repo = Repository(repositoryFolder)
                repo.init().fold(
                    (errorMessage) => UI.printError(errorMessage),
                    (successMessage) => UI.printSuccess(successMessage)
                )
            }
        }
    }

    @impure
    def call(config: Config, command: (Repository, Config) => Either[String, String]): Unit = {
        val currentFolder = Util.currentFolder
        Repository.findRepository(currentFolder) match {
            case None => UI.printError("Not a " + Repository.directoryName + " repository.")
            case Some(repositoryFolder) => {
                val repoFolder = Repository(repositoryFolder)

                command(repoFolder, config) match {
                    case Left(value) => UI.printError(value)
                    case Right(value) => UI.printSuccess(value)
                }
            }
        }
    }

    def main(args : Array[String]): Unit = {
        Parser().parse(args, Config()) match {
            case Some(config) => config.mode match {

                case "init" => init()
                case "add" => call(config, Command.add)
                case "commit" => call(config, Command.commit)
                case "status" => call(config, Command.status)
                case "checkout" => call(config, Command.checkout)
                case "branch" => config.list match {
                    case true => call(config, Command.listBranchAndTags)
                    case false => call(config, Command.createBranch)
                }
                case "tag" => call(config, Command.createTag)
                case "log" => config.patch match {
                    case true => call(config, Command.logPatch)
                    case false => config.stat match {
                        case true => call(config, Command.logStat)
                        case false => call(config, Command.log)
                    }
                }
                case "diff" => call(config, Command.diff)
                case _ =>
            }
            case _ =>
        }
    }
}
