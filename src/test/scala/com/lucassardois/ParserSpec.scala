package com.lucassardois

import org.scalatest._

class ParserSpec extends FlatSpec {

  "The parser" should "handle empty commands" in {
    assert(Parser.parse(Array("")) == Parser.Empty)
  }

  it should "handle non existing commands" in {
    assert(Parser.parse(Array("boo far hoo")) == Parser.NotACommand)
  }
}