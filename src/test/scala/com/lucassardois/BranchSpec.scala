package com.lucassardois

import org.scalatest._
import better.files._
import java.util.UUID.randomUUID

class BranchSpec extends FlatSpec {

    "A branch" should "be creatable" in {
        val branch = new Branch("master", NoParentCommit)
        assert(branch.name == "master")
        assert(branch.commit == NoParentCommit)
    }

    it should "be writable" in {
        val testFile = Test.getRandomFolder()
        testFile.createDirectories()
        val branchName = "master"
        val branch = new Branch(branchName, NoParentCommit)
        val branchFile = testFile/branchName

        IOBranch.write(testFile, branch)
        assert(branchFile.isRegularFile)
        testFile.delete()
    }

    it should "be multi writable" in {
        val testFile = Test.getRandomFolder()
        testFile.createDirectories()
        val branchName = List("1", "2", "3")
        val branches = branchName.map( (name) => {
            new Branch(name, NoParentCommit)
        })

        IOBranch.writeAll(testFile, branches)

        branches.foreach( (branch) => {
            val branchFile = testFile/branch.name
            assert(branchFile.isRegularFile)
        })

        testFile.delete()
    }

    it should "be readable" in {
        val testFile = Test.getRandomFolder()
        testFile.createDirectories()
        val branchName = "master"
        val writedBranch = new Branch(branchName, NoParentCommit)
        IOBranch.write(testFile, writedBranch)
        val readedBranch = IOBranch.read(testFile/branchName)
        assert(writedBranch.name == branchName)
        testFile.delete()
    }
}