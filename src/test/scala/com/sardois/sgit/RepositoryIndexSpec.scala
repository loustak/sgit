package com.sardois.sgit

import org.scalatest._

class RepositoryIndexSpec extends FlatSpec {

    "A repository index" should "be able to add files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)

        Test.handleException( () => {
            IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))
        })

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

    it should "be able to add nested files" in {
        val repo = IORepositoryTest.init()
        val nested = (repo.parent/"nested").createDirectories()
        val veryNested = (nested/"nestedAgain").createDirectories()
        val file = IOTest.createRandomFile(veryNested)

        Test.handleException( () => {
            IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))
        })

        IORepositoryTest.delete(repo)
    }

    it should "write added files as blobs" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)

        IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))

        IORepositoryTest.delete(repo)
    }

    it should "not write two identical files as two blobs" in {
        // But, it should still write two index entry pointing to only one blob
        val repo = IORepositoryTest.init()
        val originalFile = IOTest.createRandomFile(repo.parent)
        // Create a copy of the previous file (to get the same sha)
        val copiedFile = (repo.parent/"copy").write(originalFile.contentAsString)

        if (Util.shaFile(copiedFile) != Util.shaFile(originalFile)) {
            fail("The copied file have a different sha than the original")
        }

        val fileList = List(originalFile.pathAsString, copiedFile.pathAsString)

        Test.handleException( () => {
            IOIndex.add(repo, repo.parent, Config(paths = fileList))
        })

        // The number of index entry should be two
        val indexFile = IOIndex.getIndexFile(repo)
        assert(indexFile.lineCount == 2)

        // Only one blob should had been created since
        // the two added files have the same sha
        val blobFolder = IOBlob.getBlobsFolder(repo)
        assert(blobFolder.list.size == 1)

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

        val filesPath = Util.filesToPath(files)
        IOIndex.add(repo, repo.parent, Config(paths = filesPath))

        Util.handleException(() => {
            val indexFile = IOIndex.getIndexFile(repo)
            val index = IOIndex.read(indexFile)

            assert(indexFile.isRegularFile)
            assert(index.size == files.size)

            None
        }) match {
            case Some(value) => fail(value)
            case None =>
        }

        IORepositoryTest.delete(repo)
    }
}