package com.lucassardois

import org.scalatest._
import better.files._

class IndexSpec extends FlatSpec {

  "A repository index" should "be able to add files" in {
    val repo = IORepositoryTest.init()
    val file = Test.createRandomFile(repo.parent)
    val fileName = file.name

    val error = IORepository.add(
      repo,
      repo.parent,
      Config(paths = Array(file.pathAsString))
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
    if (!lines.isDefinedAt(0)) {
      fail("The index file is empty")
    }

    val entry = lines(0)
    val split = entry.split(" ")
    if (split.size < 2) {
      fail("Invalid entry in the index")
    }

    val path = split(0)
    val sha = split(1)

    assert(path == Repository.pathFromRepo(repo.parent, file))
    assert(sha == file.sha256)

    IORepositoryTest.delete(repo)
  }

  it should "be readable" in {
    val repo = IORepositoryTest.init()
    val dir = repo.parent
    val files = List(
      Test.createRandomFile(dir),
      Test.createRandomFile(dir),
      Test.createRandomFile(dir)
    )

    val filesPath = files.map( (file) => {
      file.pathAsString
    }).toArray

    val error = IORepository.add(
      repo,
      repo.parent,
      Config(paths = filesPath)
    )

    error match {
      case Some(error) => fail(error)
      case _ =>
    }

    val either = IORepository.readIndex(repo)

    either match {
      case Left(error) => fail(error)
      case Right(value) => {
        val file = value._1
        val mapIndex = value._2

        if (!file.isRegularFile) {
          fail("Index file is invalid.")
        }

        assert(mapIndex.size == files.size)
      }
    }

    IORepositoryTest.delete(repo)
  }

  it should "be removable" in {
    val repo = IORepositoryTest.init()
    val dir = repo.parent
    val file = Test.createRandomFile(dir)
    val filePath = file.pathAsString

    IORepository.add(
      repo,
      dir,
      Config(paths = Array(filePath))
    )

    val error = IORepository.remove(
      repo,
      dir,
      Config(paths = Array(filePath))
    )

    error match {
      case Some(error) => fail(error)
      case _ =>
    }
  }
}