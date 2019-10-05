package com.lucassardois

import org.scalatest._
import java.util.UUID.randomUUID

object IORepositoryTest extends FlatSpec {

    /* Used to get the path to the repository in test conditions */
    def getRepositoryPath(): String = "test/" + randomUUID()

    /* Helper function to create repository trough Repository.init() */
    @impure
    def init(path: String = getRepositoryPath()) = {
        val either = IORepository.init(path)
        either.getOrElse(fail("Failed to init repo."))
    }
}