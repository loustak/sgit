package com.sardois.sgit

import org.scalatest._

class IndexSpec extends FlatSpec {

    "A repository index" should "be able to add files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)

        val error = IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))

        error match {
            case Some(error) => fail(error)
            case _ =>
        }

        val index = repo/Repository.getIndexPath()
        if (!index.exists) {
            fail("The index doesn't exists")
        }

        val lines = index.lines().toList
        if (lines.size < 1) {
            fail("The number of entries in the index file is incorrect")
        }

        val entry = lines(0)
        val split = entry.split(" ")
        if (split.size < 2) {
            fail("Invalid entry in the index")
        }

        val path = split(0)
        val sha = split(1)

        assert(path == Repository.relativePathFromRepo(repo, file))
        assert(sha == Util.shaFile(file))

        IORepositoryTest.delete(repo)
    }

    it should "write added files as blobs" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)

        IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))

        IORepositoryTest.delete(repo)
    }

    it should "returns an error when adding non existing files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val filePath = file.pathAsString
        file.delete()

        val option = IOIndex.add(repo, repo.parent, Config(paths = List(filePath)))

        option match {
            case Some(_) => succeed
            case ret => fail("No error message returned, the return was: " + ret.toString)
        }

        IORepositoryTest.delete(repo)
    }

    it should "be readable" in {
        val repo = IORepositoryTest.init()
        val dir = repo.parent
        val files = List(
            IOTest.createRandomFile(dir),
            IOTest.createRandomFile(dir),
            IOTest.createRandomFile(dir)
        )

        val filesPath = files.map( (file) => {
            file.pathAsString
        })

        IOIndex.add(repo, repo.parent, Config(paths = filesPath))

        Util.handleException(() => {
            val indexFile = IOIndex.getIndexFile(repo)
            val mapIndex = IOIndex.read(indexFile)

            assert(indexFile.isRegularFile)
            assert(mapIndex.size == files.size)

            None
        }) match {
            case Some(value) => fail(value)
            case None =>
        }

        IORepositoryTest.delete(repo)
    }
}