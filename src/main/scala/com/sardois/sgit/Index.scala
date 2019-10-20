package com.sardois.sgit

import better.files.File
import com.sardois.sgit.Diff.Diffs

import scala.annotation.tailrec

trait Index extends IO {

    val mapIndex: Map[String, String]

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
        mapIndex.keys.map(key => {
            key + " " + mapIndex(key)
        }).mkString(System.lineSeparator())
    }

    def sha: String = {
        Util.shaString(serialize)
    }

    def newfiles(otherIndex: Index): List[String] = {
        mapIndex.keys.filter(key => {
            !otherIndex.mapIndex.contains(key)
        }).toList
    }

    def modified(otherIndex: Index): List[String] = {
        otherIndex.mapIndex.keys.filter(key => {
            if (mapIndex.contains(key)) {
                otherIndex.mapIndex(key) != mapIndex(key)
            } else false
        }).toList
    }

    def deleted(otherIndex: Index): List[String] = {
        mapIndex.keys.filter(key => {
            !otherIndex.mapIndex.contains(key)
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

    @impure
    def diff(repository: Repository, newIndex: Index, oldIndex: Index): Either[String, Map[String, Diffs]] = {
        val blobFolder = repository.blobsFolder

        IO.handleIOException(() => {

            val r1 = newIndex.mapIndex.keys.map(path => {
                val newSha = newIndex.mapIndex(path)
                val newBlobFile = blobFolder/newSha

                if (oldIndex.mapIndex.contains(path)) {
                    val oldSha = oldIndex.mapIndex(path)

                    if (newSha != oldSha) {
                        val oldBlobFile = blobFolder/oldSha

                        val lines1 = newBlobFile.lines.toVector
                        val lines2 = oldBlobFile.lines.toVector

                        // Get the diff between the two modified files
                        (path -> Diff.diff(lines1, lines2))
                    } else {
                        // No changes
                        (path -> Vector())
                    }

                } else {
                    val lines1 = newBlobFile.lines.toVector
                    (path -> Diff.diff(lines1, Vector()))
                }
            })

            val r2 = oldIndex.mapIndex.keys.map(path => {
                val oldSha = oldIndex.mapIndex(path)
                val oldBlobFile = blobFolder/oldSha

                if (!newIndex.mapIndex.contains(path)) {
                    val lines2 = oldBlobFile.lines.toVector
                    (path -> Diff.diff(Vector(), lines2))
                } else {
                    ("" -> Vector())
                }
            })

            Right((r1 ++ r2).toMap)
        })
    }
}

case class CommitedIndex(repository: Repository, mapIndex: Map[String, String]) extends Index {

    override val file: File = repository.indexesFolder/sha

    def add(relativePath: String, sha: String): Index = {
        CommitedIndex(repository, mapIndex + (relativePath -> sha))
    }

    def remove(relativePath: String): Index = {
        val newMap = mapIndex.filter(tuple => {
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

case class NotCommitedIndex(repository: Repository, mapIndex: Map[String, String]) extends Index {

    override val file: File = repository.indexFile

    def add(relativePath: String, sha: String): Index = {
        NotCommitedIndex(repository, mapIndex + (relativePath -> sha))
    }

    def remove(relativePath: String): Index = {
        val newMap = mapIndex.filter(tuple => {
            val key = tuple._1
            !(key == relativePath || File(key).isChildOf(File(relativePath)))
        })

        NotCommitedIndex(repository, newMap)
    }

    def toStagedIndex(): CommitedIndex = {
        CommitedIndex(repository, mapIndex)
    }

    def untracked(relativePaths: List[String]): List[String] = {
        relativePaths.filter( path => {
            !mapIndex.contains(path)
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
