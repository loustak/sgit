package com.sardois.sgit

import better.files.File

import scala.annotation.tailrec

trait Index extends IO {

    val map: Map[String, String]

    def add(relativePath: String, sha: String): Index

    def addAll(list: List[(String, String)]): Index = {
        @tailrec
        def rec(list: List[(String, String)], index: Index): Index = {
            list match {
                case ::(head, next) => {
                    val relativePath = head._1
                    val sha = head._2
                    rec(next, index.add(relativePath, sha))
                }
                case Nil => index
            }
        }

        rec(list, this)
    }

    def remove(relativePath: String): Index

    def removeAll(list: List[String]): Index = {
        @tailrec
        def rec(list: List[String], index: Index): Index = {
            list match {
                case ::(head, next) => {
                    rec(next, index.remove(head))
                }
                case Nil => index
            }
        }

        rec(list, this)
    }

    override def serialize: String = {
        map.keys.map( key => {
            key + " " + map(key)
        }).mkString(System.lineSeparator())
    }

    def sha: String = {
        Util.shaString(serialize)
    }

    def newfiles(otherIndex: Index): Iterable[String] = {
        map.keys.filter( key => {
            !otherIndex.map.contains(key)
        })
    }

    def modified(otherIndex: Index): Iterable[String] = {
        otherIndex.map.keys.filter( key => {
            if (map.contains(key)) {
                otherIndex.map(key) != map(key)
            } else false
        })
    }

    def deleted(otherIndex: Index): Iterable[String] = {
        map.keys.filter( key => {
            !otherIndex.map.contains(key)
        })
    }
}

object Index {

    def linesToMap(lines: List[String]): Map[String, String] = {
        lines.map( line => {
            val split = line.split(" ")
            val path = split(0)
            val sha = split(1)
            (path -> sha)
        }).toMap
    }
}

case class StagedIndex(repository: Repository, map: Map[String, String]) extends Index {

    override val file: File = repository.indexesFolder/sha

    def add(relativePath: String, sha: String): Index = {
        StagedIndex(repository, map + (relativePath -> sha))
    }

    def remove(relativePath: String): Index = {
        val newMap = map.filter( tuple => {
            val key = tuple._1
            !(key == relativePath || File(key).isChildOf(File(relativePath)))
        })

        StagedIndex(repository, newMap)
    }
}

object StagedIndex {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, StagedIndex] = {
        val lines = str.linesIterator.toList
        val map = Index.linesToMap(lines)
        Right(StagedIndex(repository, map))
    }
}

case class NotStagedIndex(repository: Repository, map: Map[String, String]) extends Index {

    override val file: File = repository.indexFile

    def add(relativePath: String, sha: String): Index = {
        NotStagedIndex(repository, map + (relativePath -> sha))
    }

    def remove(relativePath: String): Index = {
        val newMap = map.filter( tuple => {
            val key = tuple._1
            !(key == relativePath || File(key).isChildOf(File(relativePath)))
        })

        NotStagedIndex(repository, newMap)
    }

    def toStagedIndex(): StagedIndex = {
        StagedIndex(repository, map)
    }

    def untracked(relativePaths: List[String]): List[String] = {
        relativePaths.filter( relativePath => {
            !map.contains(relativePath)
        })
    }
}

object NotStagedIndex {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, NotStagedIndex] = {
        val lines = str.linesIterator.toList
        val map = Index.linesToMap(lines)
        Right(NotStagedIndex(repository, map))
    }
}
