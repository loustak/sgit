package com.sardois.sgit

import org.scalatest._

class IndexSpec extends FlatSpec {

    "A repository index" should "be able to add files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)

        val error = IOIndex.add(
            repo,
            repo.parent,
            Config(paths = List(file.pathAsString))
        )

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

    it should "returns an error when adding non existing files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val filePath = file.pathAsString
        file.delete()

        val error = IOIndex.add(
            repo,
            repo.parent,
            Config(paths = List(filePath))
        )

        error match {
            case Some(error) => succeed
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

        IOIndex.add(
            repo, repo.parent, Config(paths = filesPath)
        ) match {
            case Some(error) => fail(error)
            case _ =>
        }

        val tuple = for {
            indexFile <- IOIndex.getIndexFile(repo)
            mapIndex <- IOIndex.read(indexFile)
        } yield (indexFile, mapIndex)

        tuple match {
            case Left(error) => fail(error)
            case Right(tuple) => {
                val indexFile = tuple._1
                val mapIndex = tuple._2

                assert(indexFile.isRegularFile)
                assert(mapIndex.size == files.size)
            }
        }

        IORepositoryTest.delete(repo)
    }

    it should "be able to remove previously added files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)

        IOIndex.add(
            repo, repo.parent, Config(paths = List(file.pathAsString))
        ) match {
            case Some(error) => fail(error)
            case _ =>
        }
        IOIndex.remove(
            repo, repo.parent, Config(paths = List(file.pathAsString))
        ) match {
            case Some(error) => fail(error)
            case _ =>
        }

        IOIndex.getIndexFile(repo) match {
            case Left(error) => fail(error)
            case Right(indexFile) => {
                val lines = indexFile.lines().toList
                assert(lines.size == 0)
            }
        }

        IORepositoryTest.delete(repo)
    }

    it should "returns an error when removing non tracked files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)

        IOIndex.remove(
            repo,
            repo.parent,
            Config(paths = List(file.pathAsString))
        ) match {
            case Some(error) => succeed
            case _ => fail("Files where still added")
        }

        IORepositoryTest.delete(repo)
    }

    it should "not stage directories, .sgit folder and the parent repository folder" in {
        val repo = IORepositoryTest.init()
        val dir = (repo.parent/"dir").createDirectories()
        val nested = (dir/"nested").createDirectories()

        IOTest.createRandomFile(repo)
        IOTest.createRandomFile(repo.parent)
        IOTest.createRandomFile(dir)
        IOTest.createRandomFile(nested)

        val files = Repository.list(repo)
        StagedFile.createAllFromFiles(repo, files) match {
            case Left(error) => fail(error)
            case Right(stagedFiles) => {
               assert(stagedFiles.size == 3)
            }
        }

        IORepositoryTest.delete(repo)
    }

    it should "returns the list of untracked files" in {
        val repo = IORepositoryTest.init()
        val dir = (repo.parent/"dir").createDirectories()
        IOTest.createRandomFile(repo.parent)
        IOTest.createRandomFile(dir)

        val mapIndex = Map[String, String]()
        val files = Repository.list(repo)

        StagedFile.createAllFromFiles(repo, files) match {
            case Left(error) => fail(error)
            case Right(stagedFiles) => {
                val untrackedFiles = Index.listUntrackedFiles(mapIndex, stagedFiles)
                assert(untrackedFiles.size == 2)
            }
        }

        IORepositoryTest.delete(repo)
    }
}