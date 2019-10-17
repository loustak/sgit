package com.sardois.sgit

import org.scalatest._

class DiffSpec extends FlatSpec {

    "The diff algorithm" should "works" in {
        println(Diff.diff("hello", "world"))
    }
}