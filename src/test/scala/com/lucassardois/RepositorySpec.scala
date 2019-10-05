package com.lucassardois

import org.scalatest._
import better.files._

class RepositorySpec extends FlatSpec {

  /* Helper function to create repository trough Repository.init() */
  def create(path: String = RepositoryTest.getRepositoryPath()) = {
    val either = Repository.init(path)
    either.getOrElse(fail("Failed to init repo."))
  }

  "The repository" should "be initializable" in {
    create()
  }

  it should "be written on disk" in {
    val repo = create()

    repo._write()
    assert(Repository.existsAt(repo.path))
    repo._delete()
  }

  it should "head should be set to the master branch after initialization" in {
    val repo = create()
    assert(repo.head.name == "master")
  }

  it should "provide an error message if this is already an sgit repo" in {
    val path = RepositoryTest.getRepositoryPath()
    val repo1 = create(path)
    repo1._write()
    val repo2 = Repository.init(path)
    repo1._delete()

    repo2 match {
      case Right(x) => fail("Repository was still created")
      case _ => succeed
    }
  }

  it should "be destroyable for tests" in {
    val repo = create()
    repo._write()
    repo._delete()
    assert(Repository.existsAt(repo.path) == false)
  }
}