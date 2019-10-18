package com.sardois.sgit

import better.files.File

object Command {

    @impure
    def add(repository: Repository, config: Config): Either[String, String] = {
        val paths = config.paths
        val files = paths.map( path => File(path))
        val relativeFiles = files.map( file => (repository.workingDirectory.relativize(file).toString)).toList

        val filesToRemove = relativeFiles

        val existingFiles = files.filter( file => file.exists)
        val nestedFiles = existingFiles.map( file => file :: file.listRecursively.toList ).toList.flatten
        val cleanedNestedFiles = nestedFiles.filter( file => !file.isDirectory)
        val filesToAdd = cleanedNestedFiles.map( file => (repository.workingDirectory.relativize(file).toString, Util.shaFile(file)))

        val blobs = filesToAdd.map( (tuple) => {
            val file = File(tuple._1)
            val sha = tuple._2
            Blob(repository, sha, file.contentAsString)
        })

        for {
            currentIndex <- repository.currentIndex
            indexAfterRemove <- Right(currentIndex.removeAll(filesToRemove))
            indexAfterAdd <- Right(indexAfterRemove.addAll(filesToAdd))
            _ <- IO.write(indexAfterAdd)
            result <- IO.writeAll(blobs)
        } yield result
    }

    @impure
    def commit(repository: Repository, config: Config): Either[String, String] = {
        val commitMessage = config.commitMessage

        for {

        }
    }

    @impure
    def listBranchAndTags(repository: Repository, config: Config): Either[String, String] = {
        for {
            branches <- repository.branches
            result <- Right(branches.mkString(System.lineSeparator()))
        } yield result
    }
}

