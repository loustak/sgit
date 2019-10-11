package com.sardois.sgit

import org.scalatest._

class IndexSpec extends FlatSpec {

    "An index" should "be able to add by line" in {
        val baseIndex = Index()
        val newIndex = baseIndex
            .addLine("e1", "s1")
            .addLine("e2", "s2")
            .addLine("e3", "s3")
        assert(newIndex.size() == 3)
    }

    it should "be able to add staged files" in {
        val baseIndex = Index()
        val newIndex = baseIndex
            .add(StagedFile("e1", "s1"))
            .add(StagedFile("e2", "s2"))
            .add(StagedFile("e3", "s3"))
        assert(newIndex.size() == 3)
    }

    it should "be able to add multiples staged files" in {
        val baseIndex = Index()
        val files = List(
            ("f1", "s1"),
            ("f2", "s2"),
            ("f3", "s3"),
        )
        val stagedFiles = StagedFiles(files)
        val newIndex = baseIndex.addAll(stagedFiles)

        assert(newIndex.size() == files.size)
        assert(newIndex.size() == stagedFiles.size)
    }
}