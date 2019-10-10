package com.sardois.sgit

import org.scalatest._

class InitSpec extends FlatSpec {

    "A initialized repository" should "be written on disk" in {
        val repo = IORepositoryTest.init()
        assert(repo.isDirectory())
        IORepositoryTest.delete(repo)
    }

    it should "create an empty index file" in {
        val repo = IORepositoryTest.init()
        val index = repo/Repository.getIndexPath()
        assert(index.isRegularFile)
        val lines = index.contentAsString()
        assert(lines == "")
        IORepositoryTest.delete(repo)
    }

    it should "have an empty objects folder" in {
        val repo = IORepositoryTest.init()
        val blobs = repo/Repository.getBlobsPath()
        assert(blobs.isDirectory)
        assert(blobs.isEmpty)
        IORepositoryTest.delete(repo)
    }

    it should "have a refs folder" in {
        val repo = IORepositoryTest.init()
        val refs = repo/Repository.getRefsPath()
        assert(refs.isDirectory())
        IORepositoryTest.delete(repo)
    }

    it should "have a refs/heads folder" in {
        val repo = IORepositoryTest.init()
        val head = repo/Repository.getHeadsPath()
        assert(head.isDirectory())
        IORepositoryTest.delete(repo)
    }

    it should "have it's head set to master in the HEAD file" in {
        val repo = IORepositoryTest.init()
        val head = repo/Repository.getHeadPath()
        val text = head.lines().toList
        assert(text(0) == "master")
        IORepositoryTest.delete(repo)
    }

    it should "have it's master branch written" in {
        val repo = IORepositoryTest.init()
        val heads = repo/Repository.getHeadsPath()
        val master = heads/"master"
        assert(master.isRegularFile)
        IORepositoryTest.delete(repo)
    }

    it should "have a refs/tags empty folder" in {
        val repo = IORepositoryTest.init()
        val tags = repo/Repository.getTagsPath()
        assert(tags.isDirectory)
        assert(tags.isEmpty)
        IORepositoryTest.delete(repo)
    }

    it should "provide an error message if trying to be init inside an sgit repo" in {
        val folder = Test.getRandomFolder()
        IORepository.init(folder)
        val error = IORepository.init(folder)
        IOTest.delete(folder)

        error match {
            case Right(_) => {
                fail("Repository was still created")
            }
            case _ => succeed
        }
    }

    it should "provide an error message if trying to be init inside a nested fodler of an sgit repo" in {
        val folder = Test.getRandomFolder()
        IORepository.init(folder)

        val nestedDirs = folder/"nestedDir"/"anotherOne"
        nestedDirs.createDirectories()

        val either = IORepository.init(nestedDirs)
        IOTest.delete(folder)

        either match {
            case Right(_) => {
                fail("Repository was still created")
            }
            case _ => succeed
        }
    }

    it should "be destroyable for tests" in {
        val repo = IORepositoryTest.init()
        IORepositoryTest.delete(repo)
        assert(repo.isDirectory() == false)
    }
}