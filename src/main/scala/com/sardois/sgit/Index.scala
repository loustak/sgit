package com.sardois.sgit

import better.files.File

import scala.annotation.tailrec

case class Index(repository: Repository, map: Map[String, String]) extends IO {

    override val file: File = repository.indexFile

    override def serialize: String = {
        map.keys.map( key => {
            key + " " + map(key)
        }).mkString(System.lineSeparator())
    }

    def add(relativePath: String, sha: String): Index = {
        Index(repository, map + (relativePath -> sha))
    }

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

    def remove(relativePath: String): Index = {
        val newMap = map.filter( tuple => {
            val key = tuple._1
            !(key == relativePath || File(key).isChildOf(File(relativePath)))
        })

        Index(repository, newMap)
    }

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
}

object Index {

    def deserialize(repository: Repository, fileName: String, str: String): Either[String, Index] = {
        val lines = str.linesIterator.toList
        val map = lines.map( line => {
            val split = line.split(" ")
            val path = split(0)
            val sha = split(1)
            (path -> sha)
        }).toMap

        Right(Index(repository, map))
    }
}


