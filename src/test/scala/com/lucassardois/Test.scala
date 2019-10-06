package com.lucassardois

import better.files._
import java.util.UUID.randomUUID

object Test {

    def getFile(): File = File("test")

    def getRandomFile(): File = getFile()/randomUUID().toString()
}