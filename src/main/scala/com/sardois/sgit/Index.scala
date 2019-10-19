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

    def newfiles(otherIndex: Index): List[String] = {
        map.keys.filter( key => {
            !otherIndex.map.contains(key)
        }).toList
    }

    def modified(otherIndex: Index): List[String] = {
        otherIndex.map.keys.filter( key => {
            if (map.contains(key)) {
                otherIndex.map(key) != map(key)
            } else false
        }).toList
    }

    def deleted(otherIndex: Index): List[String] = {
        map.keys.filter( key => {
            !otherIndex.map.contains(key)
        }).toList
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

case class CommitedIndex(repository: Repository, map: Map[String, String]) extends Index {

    override val file: File = repository.indexesFolder/sha

    def add(relativePath: String, sha: String): Index = {
        CommitedIndex(repository, map + (relativePath -> sha))
    }

    def remove(relativePath: String): Index = {
        val newMap = map.filter( tuple => {
            val key = tuple._1
            !(key == relativePath || File(key).isChildOf(File(relativePath)))
        })

        CommitedIndex(repository, newMap)
    }
}

object CommitedIndex {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, CommitedIndex] = {
        val lines = str.linesIterator.toList
        val map = Index.linesToMap(lines)
        Right(CommitedIndex(repository, map))
    }

    def empty(repository: Repository): CommitedIndex = {
        CommitedIndex(repository, Map[String, String]())
    }
}

case class NotCommitedIndex(repository: Repository, map: Map[String, String]) extends Index {

    override val file: File = repository.indexFile

    def add(relativePath: String, sha: String): Index = {
        NotCommitedIndex(repository, map + (relativePath -> sha))
    }

    def remove(relativePath: String): Index = {
        val newMap = map.filter( tuple => {
            val key = tuple._1
            !(key == relativePath || File(key).isChildOf(File(relativePath)))
        })

        NotCommitedIndex(repository, newMap)
    }

    def toStagedIndex(): CommitedIndex = {
        CommitedIndex(repository, map)
    }

    def untracked(relativePaths: List[String]): List[String] = {
        relativePaths.filter( path => {
            !map.contains(path)
        })
    }
}

object NotCommitedIndex {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, NotCommitedIndex] = {
        val lines = str.linesIterator.toList
        val map = Index.linesToMap(lines)
        Right(NotCommitedIndex(repository, map))
    }
}
