package com.lucassardois

import org.scalatest._
import better.files._
import java.util.UUID.randomUUID

object IORepositoryTest extends FlatSpec {

    /* Used to get the path to the repository in test conditions.
    we isolate each test repo in a separate folder to make test
    frictionless as possible. */
    def getRepositoryFile(): File = {
        File("test/" + randomUUID() + "/" + Repository.getDirectoryName())
    }

    /* Helper function to create repository trough Repository.init() */
    @impure
    def init(file: File = getRepositoryFile()): Repository = {
        IORepository.init(file) match {
            case Left(error) => {
                fail("Failed to init repo: " + error)
            }
            case Right(repo) => {
                repo
            }
        }
    }

    /* Delete all the files in a repository, this function
    is not available for the user but only for tests.
    This function is unpure since it only manage files. */
    @impure
    def delete(repo: Repository) = {
        repo.file.parent.delete()
    }
}