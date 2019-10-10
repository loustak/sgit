package com.sardois.sgit

import org.scalatest._

class RepositorySpec extends FlatSpec {

    "A repository" should "be able to know if a file is inside the .sgit folder or not" in {
        val repo = IORepositoryTest.init()
        val fileInParentFolder = IOTest.createRandomFile(repo.parent)
        val fileInRepo = IOTest.createRandomFile(repo)

        assert(fileInParentFolder.isChildOf(repo) == false)
        assert(fileInRepo.isChildOf(repo))

        IORepositoryTest.delete(repo)
    }

    it should "be able to list all it's files except the one in the .sgit folder" in {
        val repo = IORepositoryTest.init()
        val file0 = IOTest.createRandomFile(repo)

        val nested = (repo.parent/"nested").createDirectories()
        val deeplyNested = (nested/"deeplyNested").createDirectories()

        val files = List(
            IOTest.createRandomFile(repo.parent),
            IOTest.createRandomFile(nested),
            IOTest.createRandomFile(deeplyNested)
        )

        val listedFiles = Repository.list(repo)
        /** Includes all the files and directories except: . (current folder) and .sgit */
        assert(listedFiles.size == 5)

        IORepositoryTest.delete(repo)
    }
}