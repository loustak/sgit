package com.lucassardois

import org.scalatest._
import better.files._

class RepositorySpec extends FlatSpec {

  "A repository" should "be initializable" in {
    val repo = IORepositoryTest.init()
    IORepositoryTest.delete(repo)
  }

  it should "be written on disk" in {
    val repo = IORepositoryTest.init()
    assert(repo.file.isDirectory())
    IORepositoryTest.delete(repo)
  }

  it should "have a refs folder" in {
    val repo = IORepositoryTest.init()
    assert(repo.getRefsFile().isDirectory())
    IORepositoryTest.delete(repo)
  }

  it should "have a refs/heads folder" in {
    val repo = IORepositoryTest.init()
    assert(repo.getHeadsFile().isDirectory())
    IORepositoryTest.delete(repo)
  }

  it should "have it's head set to the master branch after initialization" in {
    val repo = IORepositoryTest.init()
    assert(repo.head.name == "master")
    IORepositoryTest.delete(repo)
  }

  it should "write it's head in the HEAD file" in {
    val repo = IORepositoryTest.init()
    val head = repo.getHeadFile()
    val text = head.lines().toList
    assert(text(0) == "master")
    IORepositoryTest.delete(repo)
  }

  it should "have it's master branch written after initialization" in {
    val repo = IORepositoryTest.init()
    val heads = repo.getHeadsFile()
    val master = heads/"master"
    assert(master.isRegularFile)
    IORepositoryTest.delete(repo)
  }

  it should "have all it's branch correctly written" in {
    val file = IORepositoryTest.getRepositoryFile()
    val branches = List(
      new Branch("1", NoParentCommit),
      new Branch("2", NoParentCommit),
      new Branch("3", NoParentCommit)
    )
    val repo = new Repository(file, branches(0), branches)
    val heads = repo.getHeadsFile()
    IORepository.write(repo)
    branches.foreach( (branch) => {
      val branchFile = heads/branch.name
      assert(branchFile.isRegularFile)
    })
    IORepositoryTest.delete(repo)
  }

  it should "provide an error message if this is already an sgit repo" in {
    val file = IORepositoryTest.getRepositoryFile()
    val repo1 = IORepository.init(file).getOrElse(fail())
    val repo2 = IORepository.init(file)
    IORepositoryTest.delete(repo1)

    repo2 match {
      case Right(x) => {
        IORepositoryTest.delete(x)
        fail("Repository was still created")
      }
      case _ => succeed
    }
  }

  it should "provide an error message if there is a parent sgit repo" in {
    val file = IORepositoryTest.getRepositoryFile()
    val repo1 = IORepository.init(file).getOrElse(fail())

    val nestedDirs = file.parent/"nestedDir"/"anotherOne"
    nestedDirs.createDirectories()

    val repo2 = IORepository.init(nestedDirs)
    IORepositoryTest.delete(repo1)

    repo2 match {
      case Right(x) => {
        IORepositoryTest.delete(x)
        fail("Repository was still created")
      }
      case _ => succeed
    }
  }

  it should "be destroyable for tests" in {
    val repo = IORepositoryTest.init()
    val file = repo.file
    IORepositoryTest.delete(repo)
    assert(file.isDirectory() == false)
  }
}