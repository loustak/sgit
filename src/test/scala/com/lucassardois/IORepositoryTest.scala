package com.lucassardois

import org.scalatest._
import better.files._
import java.util.UUID.randomUUID

object IORepositoryTest extends FlatSpec {

    /* Helper function to create repository trough Repository.init() */
    @impure
    def init(folder: File = Test.getRandomFolder()): File = {
        val either = IORepository.init(folder)

        either match {
            case Left(error) => {
                fail("Failed to init repo: " + error)
            }
            case Right(repoFolder) => repoFolder
        }
    }

    @impure
    def delete(repoFolder: File) = {
        repoFolder.parent.delete()
    }
}