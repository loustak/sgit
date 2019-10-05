package com.lucassardois

import org.scalatest._
import better.files._

class BranchesSpec extends FlatSpec {

    "Branches" should "be written on repo init" in {
        val repo = IORepositoryTest.init()
        val branchesPath = repo.getBranchesPath()
        assert(repo.branchesFileExists())
        IORepository.delete(repo)
    }

    it should "have the correct format" in {
        val repo = IORepositoryTest.init()
        val branches = repo.branches
        val branchesPath = repo.getBranchesPath()

        val file = File(branchesPath)
        file.lines().foreach( (line) => {
            val splited = line.split(" ")

            assert(splited.isDefinedAt(0))
            assert(splited.isDefinedAt(1))
        })

        IORepository.delete(repo)
    }
}