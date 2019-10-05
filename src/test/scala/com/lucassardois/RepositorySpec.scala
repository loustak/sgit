package com.lucassardois

import org.scalatest._
import better.files._

class RepositorySpec extends FlatSpec {

  "The repository" should "be initializable" in {
    val path = RepositoryTest.getRepositoryPath()
    val file = File(path)

    RepositoryConstructor(path)

    assert(file.isDirectory())
  }
}