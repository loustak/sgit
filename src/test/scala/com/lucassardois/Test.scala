package com.lucassardois

import better.files._
import java.util.UUID.randomUUID

object Test {

    def getTestFolder(): File = File("test")

    def getRandomFolder(): File = getTestFolder()/randomUUID().toString()
}

object IOTest {

    @impure
    def delete(file: File): Unit = {
        file.delete()
    }
}