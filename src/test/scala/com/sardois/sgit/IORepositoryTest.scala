package com.sardois.sgit

import better.files._
import org.scalatest._

object IORepositoryTest extends FlatSpec {

    /* Helper function to create repository trough Repository.init() */
    @impure
    def init(folder: File = Test.getRandomFolder): File = {
        val either = IORepository.init(folder)

        either match {
            case Left(error) =>
                fail("Failed to init repo: " + error)
            case Right(repoFolder) => repoFolder
        }
    }

    @impure
    def delete(repoFolder: File): File = {
        repoFolder.parent.delete()
    }
}