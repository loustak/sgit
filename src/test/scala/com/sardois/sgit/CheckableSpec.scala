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

    it should "be able to be listed" in {
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
}