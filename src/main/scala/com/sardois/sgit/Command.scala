package com.sardois.sgit

import better.files.File

object Command {

    @impure
    def add(repository: Repository, config: Config): Either[String, String] = {
        val files = config.paths
            .map(path => File(path))
            .toList

        val filesToRemove = files.map(file => repository.relativize(file))

        val filesToAdd = files
            .filter(file => file.exists)
            .map(file => file :: file.listRecursively.toList).flatten
            .filter(file => {
                !(
                    file.isDirectory ||
                    file == repository.repositoryFolder ||
                    file.isChildOf(repository.repositoryFolder)
                )
            })
            .map(file => (repository.relativize(file), Util.shaFile(file)))

        val blobs = filesToAdd.map( (tuple) => {
            val file = File(tuple._1)
            val sha = tuple._2
            Blob(repository, sha, file.contentAsString)
        })

        for {
            currentIndex <- repository.notCommitedCurrentIndex
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
            currentIndex <- repository.notCommitedCurrentIndex
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
        val modifiedIndex = repository.listPotentiallyModifiedFilesAsIndex()
        val deletedIndex = repository.listPotentiallyDeletedFilesAsIndex()
        val untrackedFiles = repository.listPotentiallyUntrackedFiles()

        val indexes = for {
            head <- repository.head
            branch <- head.branch
            notCommitedCurrentIndex <- repository.notCommitedCurrentIndex
            lastCommitedIndex <- repository.lastCommitedIndex
        } yield (branch, notCommitedCurrentIndex, lastCommitedIndex)

        indexes match {
            case Left(value) => Left(value)
            case Right(tuple) => {
                val branch = tuple._1
                val uncommitedCurrentIndex = tuple._2
                val lastCommitedIndex = tuple._3

                val finalStr = UI.status(
                    branch.name,
                    uncommitedCurrentIndex.newfiles(lastCommitedIndex),
                    uncommitedCurrentIndex.modified(lastCommitedIndex),
                    lastCommitedIndex.deleted(uncommitedCurrentIndex),
                    uncommitedCurrentIndex.modified(modifiedIndex),
                    uncommitedCurrentIndex.deleted(deletedIndex),
                    uncommitedCurrentIndex.untracked(untrackedFiles)
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

    @impure
    def checkout(repository: Repository, config: Config): Either[String, String] = {
        repository.hasUncommitedChanges.map( result => {
            if (result) {
                return Left("You have uncommited changes, please first commit before checkout.")
            }
        })

        Right("Checkout ready")
    }
}

