package com.lucassardois

import org.scalatest._
import java.util.UUID.randomUUID

object RepositoryTest {

    /* Used to get the path to the repository in test conditions */
    def getRepositoryPath(): String = "test/" + randomUUID()
}