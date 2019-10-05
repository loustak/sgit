package com.lucassardois

import org.scalatest._
import better.files._

class BranchSpec extends FlatSpec {

    "A branch" should "be creatable" in {
        val b = new Branch("master", NoParentCommit)
    }

    it should "have the correct name" in {
        val name = "master"
        val b = new Branch(name, NoParentCommit)
        assert(b.name == name)
    }

    it should "have the correct commit" in {
        val commit = NoParentCommit
        val b = new Branch("master", commit)
        assert(b.commit == commit)
    }
}