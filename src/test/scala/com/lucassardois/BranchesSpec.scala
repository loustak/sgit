package com.lucassardois

import org.scalatest._
import better.files._

class BranchesSpec extends FlatSpec {

    "Branches" should "be written on repo init" in {
        val repo = IORepositoryTest.init()
        assert(repo.branchesFileExists())
        IORepositoryTest.delete(repo)
    }

    it should "have the correct format" in {
        val repo = IORepositoryTest.init()
        val branches = repo.branches
        val file = repo.getBranchFile()

        file.lines().foreach( (line) => {
            val splited = line.split(" ")

            assert(splited.isDefinedAt(0))
            assert(splited.isDefinedAt(1))
        })

        IORepositoryTest.delete(repo)
    }
}