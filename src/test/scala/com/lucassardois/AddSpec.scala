package com.lucassardois

import org.scalatest._
import better.files._

class AddSpec extends FlatSpec {

  "A repository" should "able to add files to track" in {
      val repo = Test.getRandomFolder()
      Repository.callInside(repo, Nil, IORepository.add)
  }
}