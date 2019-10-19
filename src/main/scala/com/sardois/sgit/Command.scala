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
            currentIndex <- repository.currentIndex
            lastCommitSha <- repository.lastCommitSha
            newCommit <- Right(Commit(repository, commitMessage, currentIndex.sha, lastCommitSha))
            head <- repository.head
            branch <- head.branch
            newBranch <- Right(branch.moveToCommit(newCommit.sha))
            _ <- IO.write(currentIndex.toStagedIndex())
            _ <- IO.write(newCommit)
            result <- IO.write(newBranch)
        } yield result
    }

    @impure
    def status(repository: Repository, config: Config): Either[String, String] = {
        val files = repository.workingDirectory.listRecursively
        val cleanedFiles = files.filter( file => {
            !(file == repository.repositoryFolder  || file.isDirectory || file.isChildOf(repository.repositoryFolder))
        })
        val relativePaths = cleanedFiles.map( file => repository.workingDirectory.relativize(file).toString).toList
        val filesModifiedToCheck = relativePaths.map( path => (path, Util.shaFile(File(path)))).toMap
        val deletedFilesToCheck = relativePaths.filter( path => File(path).exists).map( path => (path , "")).toMap

        val modifiedIndex = NotStagedIndex(repository, filesModifiedToCheck)
        val deletedIndex = NotStagedIndex(repository, deletedFilesToCheck)

        val indexes = for {
            notStagedIndex <- repository.currentIndex
            lastCommit <- repository.lastCommit
            stagedIndex <- lastCommit.index
        } yield (notStagedIndex, stagedIndex)

        indexes match {
            case Left(value) => Left(value)
            case Right(tuple) => {
                val notStagedIndex = tuple._1
                val stagedIndex = tuple._2

                val newStagedFiles = notStagedIndex.newfiles(stagedIndex)
                val modifiedStagedFiles = notStagedIndex.modified(stagedIndex)
                val deletedStagedFiles = notStagedIndex.deleted(stagedIndex)
                val modifiedNotStagedFiles = notStagedIndex.modified(modifiedIndex)
                val deletedNotStagedFiles = notStagedIndex.deleted(deletedIndex)
                val untrackedFiles = notStagedIndex.untracked(relativePaths)

                val newLine = System.lineSeparator()
                val newStagedFilesStr = newStagedFiles.map( str => "added: " + str)
                val modifiedStagedFilesStr = modifiedStagedFiles.map( str => "modified: " + str)
                val deletedStagedFilesStr = deletedStagedFiles.map( str => "deleted: " + str)
                val modifiedNotStagedFilesStr = modifiedNotStagedFiles.map( str => "modified: " + str)
                val deletedNotStagedFilesStr = deletedNotStagedFiles.map( str => "deleted: " + str)

                val stagedStr = List(
                    "Staged changes:",
                    newStagedFilesStr,
                    modifiedStagedFilesStr,
                    deletedStagedFilesStr
                ).mkString(newLine)
                val notStagedStr = List(
                    "Not staged changes:",
                    modifiedNotStagedFilesStr,
                    deletedNotStagedFilesStr
                )
                val untrackedFilesStr = List(
                    "Untracked files:",
                    untrackedFiles.mkString(newLine)
                )

                Right(List(stagedStr, notStagedStr, untrackedFilesStr).mkString(newLine))
            }
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

