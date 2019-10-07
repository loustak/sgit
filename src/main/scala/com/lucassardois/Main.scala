package com.lucassardois

import better.files._

object Main {
  
  def main(args : Array[String]): Unit = {
    Parser().parse(args, Config()) match {
      case Some(config) => config.mode match {
        case "init" => {
          val repoFolder = File(Repository.getDirectoryName())
          IORepository.init(repoFolder)
        }
        case _ =>
      }
      case _ => print("Invalid command.")
    }
  }
}
