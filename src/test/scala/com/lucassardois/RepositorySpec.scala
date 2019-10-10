package com.lucassardois

import org.scalatest._

class RepositorySpec extends FlatSpec {

    "A repository" should "be able to know if a file is inside the .sgit folder or not" in {
        val repo = IORepositoryTest.init()
        val fileInParentFolder = Test.createRandomFile(repo.parent)
        val fileInRepo = Test.createRandomFile(repo)

        assert(fileInParentFolder.isChildOf(repo) == false)
        assert(fileInRepo.isChildOf(repo))

        IORepositoryTest.delete(repo)
    }

    it should "be able to list all it's files except the one in the .sgit folder" in {
        val repo = IORepositoryTest.init()
        val file0 = Test.createRandomFile(repo)

        val nested = (repo.parent/"nested").createDirectories()
        val deeplyNested = (nested/"deeplyNested").createDirectories()

        val files = List(
            Test.createRandomFile(repo.parent),
            Test.createRandomFile(nested),
            Test.createRandomFile(deeplyNested)
        )

        val listedFiles = Repository.list(repo)
        /** Includes all the files and directories except: . (current folder) and .sgit */
        assert(listedFiles.size == 5)
    }
}