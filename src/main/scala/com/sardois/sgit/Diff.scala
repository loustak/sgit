package com.sardois.sgit

import better.files.File

import scala.annotation.tailrec

object Diff {

    def diff(file1: File, file2: File) = {
        val lines1 = lines(file1)
        val lines2 = lines(file2)

    }

    def lines(file: File): Map[Int, String] = {
        @tailrec
        def rec(lines: List[String], lineNumber: Int, map: Map[Int, String]): Map[Int, String] = {
            lines match {
                case ::(head, next) => {
                    val newMap = map + (lineNumber -> head)
                    rec(next, lineNumber + 1, newMap)
                }
                case Nil => map
            }
        }

        rec(file.lines.toList, 1, Map[Int, String]())
    }

    def diff(s1: String, s2: String): List[String] =
        (s1, s2).zipped.collect {
            case (x, y) if x != y => s"$x != $y"
        }.toList ++
            s1.drop(s2.length).map(x => s"$x is undefined") ++
            s2.drop(s1.length).map(y => s"$y is missing")
}
