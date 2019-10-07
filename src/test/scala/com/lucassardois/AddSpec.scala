package com.lucassardois

import org.scalatest._
import better.files._

class AddSpec extends FlatSpec {

  "A repository" should "able to add one file to track" in {
    val repo = IORepositoryTest.init()
    val file = Test.createRandomFile(repo.parent)
    val fileName = file.name

    val error = IORepository.add(repo, repo.parent, Config(paths = Array(file.pathAsString)))

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

    val sha = split(0)
    val path = split(1)

    assert(sha == file.sha256)
    assert(path == Repository.pathFromRepo(repo.parent, file))

    IORepositoryTest.delete(repo)
  }
}