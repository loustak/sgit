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
            head <- repository.head
            branch <- head.branch
            notStagedIndex <- repository.currentIndex
            lastCommit <- repository.lastCommit
            stagedIndex <- lastCommit.index
        } yield (branch, notStagedIndex, stagedIndex)

        indexes match {
            case Left(value) => Left(value)
            case Right(tuple) => {
                val branch = tuple._1
                val notStagedIndex = tuple._2
                val stagedIndex = tuple._3

                val finalStr = UI.status(
                    branch.name,
                    notStagedIndex.newfiles(stagedIndex),
                    notStagedIndex.modified(stagedIndex),
                    stagedIndex.deleted(notStagedIndex),
                    notStagedIndex.modified(modifiedIndex),
                    notStagedIndex.deleted(deletedIndex),
                    notStagedIndex.untracked(relativePaths)
                )

                Right(finalStr)
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

