package com.sardois.sgit

import org.scalatest._

class CheckableSpec extends FlatSpec {

    "IOCheckable" should "be able to create a branch" in {
        val repo = IORepositoryTest.init()
        val branchName = "Test"

        IOCheckable.create(repo, repo.parent, Config(branchName = branchName))

        val branchesFolder = IOCheckable.getBranchesFolder(repo)
        val branches = IOCheckable.list(branchesFolder)
        assert(branches.size == 2)

        IORepositoryTest.delete(repo)
    }

    it should "be able to list branches" in {
        val repo = IORepositoryTest.init()
        val names = List("branch1", "branch2")

        names.foreach( name => {
            IOCheckable.create(repo, repo.parent, Config(branchName = name))
        })

        val branchesFolder = IOCheckable.getBranchesFolder(repo)
        val branches = IOCheckable.list(branchesFolder)
        assert(branches.size == names.size + 1)

        IORepositoryTest.delete(repo)
    }

    it should "be able to create a tag" in {
        val repo = IORepositoryTest.init()
        val tagName = "Test"

        IOCheckable.create(repo, repo.parent, Config(tagName = tagName))

        val tagsFolder = IOCheckable.getTagsFolder(repo)
        val tags = IOCheckable.list(tagsFolder)

        assert(tags.size == 1)

        IORepositoryTest.delete(repo)
    }
}