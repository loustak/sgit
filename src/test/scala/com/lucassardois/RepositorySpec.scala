package com.lucassardois

import org.scalatest._
import better.files._

class RepositorySpec extends FlatSpec {


  "A repository" should "be initializable" in {
    IORepositoryTest.init()
  }

  it should "be written on disk" in {
    val repo = IORepositoryTest.init()

    assert(Repository.existsAt(repo.path))
    IORepository.delete(repo)
  }

  it should "have it's head set to the master branch after initialization" in {
    val repo = IORepositoryTest.init()
    assert(repo.head.name == "master")
  }

  it should "provide an error message if this is already an sgit repo" in {
    val path = IORepositoryTest.getRepositoryPath()
    val repo1 = IORepository.init(path).getOrElse(fail())
    val repo2 = IORepository.init(path)
    IORepository.delete(repo1)

    repo2 match {
      case Right(x) => fail("Repository was still created")
      case _ => succeed
    }
  }

  it should "be destroyable for tests" in {
    val repo = IORepositoryTest.init()
    IORepository.delete(repo)
    assert(Repository.existsAt(repo.path) == false)
  }
}