package com.sardois.sgit

import better.files.File
import com.sardois.sgit.Diff.Diffs
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
        def rec2(l: String, l2: Vector[String], ln1: Int, ln2: Int, diffs: Diffs): (Vector[String], Diffs) = {
            if (l2.isEmpty) (l2, diffs :+ Diff(ln1, DiffEnum.ADD, l))
            else if (l == l2.head) (l2.tail, diffs)
            else {
                val newDiff = Diff(ln2, DiffEnum.REMOVE, l2.head)
                rec2(l, l2.tail, ln1, ln2 + 1, diffs :+ newDiff)
            }
        }

        @tailrec
        def rec(l1: Vector[String], l2: Vector[String], lineNumber: Int, diffs: Diffs): Diffs = {
            if (l1.isEmpty) return diffs

            val h1 = l1.head

            if (l2.isEmpty) {
                rec(l1.tail, l2, lineNumber + 1, diffs :+ Diff(lineNumber, DiffEnum.ADD, h1))
            } else if (h1 == l2.head) {
                rec(l1.tail, l2.tail, lineNumber + 1, diffs)
            } else {
                val (nl2, newDiff) = rec2(h1, l2, lineNumber, lineNumber, Vector())
                rec(l1.tail, nl2, lineNumber + 1, diffs :++ newDiff)
            }

        }

        rec(l1, l2, 0, Vector())
    }
}
