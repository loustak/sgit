package com.sardois.sgit

import org.scalatest._

class CommitSpec extends FlatSpec {

    "A commit" should "be doable" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val message = "Test commit"

        IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))
        IOCommit.commit(repo, repo.parent, Config(commitMessage = message))

        val indexFile = IOIndex.getIndexFile(repo)
        val index = IOIndex.read(indexFile)
        val indexSha = index.sha()
        val commit = Commit(message, indexSha, Commit.rootCommitSha())
        val newCommitFile = repo/Repository.getCommitsPath()/commit.sha()

        // The format of the commit is correct
        assert(newCommitFile.contentAsString == commit.toString)

        // The index was saved
        val savedIndexedFile = IOIndex.getIndexesFolder(repo)/indexSha
        assert(savedIndexedFile.exists)
        assert(savedIndexedFile.contentAsString == index.toString)

        // The commit sha referenced by the HEAD was updated
        val newCommitSha = IOHead.getPointedCommitSha(repo)
        assert(newCommitSha == commit.sha())

        IORepositoryTest.delete(repo)
    }

    it should "be readable" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val message = "Test commit"

        IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))
        IOCommit.commit(repo, repo.parent, Config(commitMessage = message))

        val commitSha = IOHead.getPointedCommitSha(repo)
        val commitsFolder = IOCommit.getCommitsFolder(repo)
        val commit = IOCommit.read(commitsFolder, commitSha)

        assert(commit.sha() == commitSha)

        IORepositoryTest.delete(repo)
    }
}