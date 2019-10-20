package com.sardois.sgit

import com.sardois.sgit.DiffEnum.DiffEnum

import scala.annotation.tailrec

case class Line(lineNumber: Int, text: String) {

}

case class Diff(lineNumber: Int, diffEnum: DiffEnum, line: String) {

    override def toString: String = {
        lineNumber.toString + " " + diffEnum.toString + " " + line
    }
}

object DiffEnum extends Enumeration {

    type DiffEnum = Value

    val ADD = Value("ADD")
    val REMOVE = Value("REMOVE")
}

object Diff {

    type Diffs = Vector[Diff]

    def apply(lineNumber: Int, diffEnum: DiffEnum, line: String): Diff = {
        new Diff(lineNumber, diffEnum, line)
    }

    def diff(l1: Vector[String], l2: Vector[String]): Diffs = {

        @tailrec
        def rec(i1: Int, i2: Int, diffs: Diffs): Diffs = {
            if (i1 == l1.length) {
                if (i2 == l2.length) return diffs
                var c = 0
                return l2.map(str => {
                    val diff = Diff(i2 + c, DiffEnum.REMOVE, str)
                    c = c + 1
                    diff
                })
            }

            val h1 = l1(i1)

            if (i2 == l2.length) {
                rec(i1 + 1, i2, diffs :+ Diff(i1, DiffEnum.ADD, h1))
            } else if (h1 == l2(i2)) {
                rec(i1 + 1, i2 + 1, diffs)
            } else {
                val idx = l2.indexOf(h1, i2)
                if (idx == -1) {
                    if (i1 == l1.length -1) {
                        // We are on the last element, remove all the remaining elements of l2
                        var c = 0
                        val newDiff = l2.slice(i2, l2.length).map(str => {
                            val diff = Diff(i2 + c, DiffEnum.REMOVE, str)
                            c = c + 1
                            diff
                        }) :+ Diff(i1, DiffEnum.ADD, h1)
                        rec(i1 + 1, i2 + c, diffs :++ newDiff)
                    } else {
                        rec(i1 + 1, i2, diffs :+ Diff(i1, DiffEnum.ADD, h1))
                    }
                } else {
                    // Found, add all the intermediate elements
                    var c = 0
                    val newDiff = l2.slice(i2, idx).map(str => {
                        val diff = Diff(i2 + c, DiffEnum.REMOVE, str)
                        c = c + 1
                        diff
                    })
                    rec(i1 + 1, idx + 1, diffs :++ newDiff)
                }
            }
        }

        rec(0, 0, Vector())
    }
}
