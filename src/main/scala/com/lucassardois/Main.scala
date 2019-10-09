package com.lucassardois

import better.files._

object Main {

  def call(args: Config, func: (File, File, Config) => Option[String]): Unit = {
    val error = Repository.callInside(args, func)

    error match {
      case Some(error) => println(error)
      case _ =>
    }
  }
  
  def main(args : Array[String]): Unit = {
    Parser().parse(args, Config()) match {
      case Some(config) => config.mode match {
        case "init" => {
          val repoFolder = File(Repository.getDirectoryName())
          val either = IORepository.init(repoFolder)

          either match {
            case Left(error) => print(error)
            case _ =>
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
      case _ => print("Unknown command.")
    }
  }
}
