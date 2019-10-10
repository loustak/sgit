package com.sardois.sgit

import better.files._

import scala.annotation.tailrec

object Test {

    def getTestFolder(): File = File("test")

    /** Return a random non existing folder */
    @tailrec
    def getRandomFolder(): File = {
        val file = getTestFolder()/("test-" + randomNumberString(8))
        if (file.exists) getRandomFolder()
        else file
    }

    @tailrec
    def getRandomFile(folder: File): File = {
        val file = folder/("file-" + randomString(8))
        if (file.exists) getRandomFile(folder)
        else file
    }

    def randomNumberString(length: Int): String = {
        scala.util.Random.alphanumeric.filter(_.isDigit).take(length).mkString
    }

    def randomString(length: Int): String = {
        scala.util.Random.alphanumeric.filter(_.isLetter).take(length).mkString
    }
}

object IOTest {

    /** Create a random file with a random content.
     * The content is a random string with no guarantee.
     * */
    @impure
    def createRandomFile(folder: File): File = {
        val file = Test.getRandomFile(folder)
        file.write(Test.randomString(20))
    }

    @impure
    def delete(file: File): Unit = {
        file.delete()
    }
}