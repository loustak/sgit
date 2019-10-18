package com.sardois.sgit

import better.files.File
import com.sardois.sgit.Diff.Diffs
import com.sardois.sgit.DiffEnum.DiffEnum

import scala.annotation.tailrec

class Line(val lineNumber: Int, val text: String) {

    def diff(otherLine: Line): Diffs = {
        if (text == otherLine.text) {
            List(Diff(lineNumber, DiffEnum.SAME, text))
        } else {
            List(
                Diff(lineNumber, DiffEnum.REMOVE, otherLine.text),
                Diff(lineNumber, DiffEnum.ADD, text)
            )
        }
    }
}

object Line {

    def apply(number: Int, text: String): Line = {
        new Line(number, text)
    }
}

class Diff(val lineNumber: Int, diffEnum: DiffEnum, val line: String) {

}

object DiffEnum extends Enumeration {

    type DiffEnum = Value

    val SAME = Value
    val ADD = Value
    val REMOVE = Value
}

object Diff {

    type Diffs = List[Diff]
    type Lines = List[Line]

    def apply(lineNumber: Int, diffEnum: DiffEnum, line: String): Diff = {
        new Diff(lineNumber, diffEnum, line)
    }

    def linesFromString(lines: Iterable[String]): Lines = {
        @tailrec
        def rec(lines: Iterable[String], lineNumber: Int, list: Lines): Lines = {
            lines match {
                case ::(head, next) => {
                    val newList = Line(lineNumber, head) :: list
                    rec(next, lineNumber + 1, newList)
                }
                case Nil => list
            }
        }

        rec(lines, 1, List())
    }

    def diff(l1: Lines, l2: Lines): Diffs = {
        /*
        @tailrec
        def rec(l1: Lines, l2: Lines, lineNumber: Int, diffs: Diffs): Diffs = {
            if (l1.isEmpty && l2.isEmpty) diffs

            val la = if (l1.isEmpty) Line(lineNumber, "") else l1.head
            val lb = if (l2.isEmpty) Line(lineNumber, "") else l2.head
            val newl1 = if (l1.isEmpty) l1 else l1.tail
            val newl2 = if (l2.isEmpty) l2 else l2.tail
            val diff = la.diff(lb)

            rec(newl1, newl2, lineNumber + 1, (diff :: diffs))
        }

        rec(l1, l2, 0, List())
         */
        ???
    }
}
