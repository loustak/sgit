package com.lucassardois

import better.files._
import java.util.UUID.randomUUID
import scala.annotation.tailrec

object Test {

    def getTestFolder(): File = File("test")

    /** Return a random non existing folder */
    @tailrec
    def getRandomFolder(): File = {
        val file = getTestFolder()/("test-" + randomNumberString(8))
        if (file.exists) return getRandomFolder()
        else file
    }

    def randomNumberString(length: Int): String = {
        scala.util.Random.alphanumeric.filter(_.isDigit).take(length).mkString
    }

    def randomString(length: Int): String = {
        scala.util.Random.alphanumeric.filter(_.isLetter).take(length).mkString
    }

    /** Create a random file with a random content.
    * The content is a random string with no guarentee. */
    @impure
    @tailrec
    def createRandomFile(folder: File): File = {
        val file = folder/("file-" + randomString(8))
        if (file.exists) return createRandomFile(folder)
        else {
            file.write(randomString(20))
        }
    }
}

object IOTest {

    @impure
    def delete(file: File): Unit = {
        file.delete()
    }
}