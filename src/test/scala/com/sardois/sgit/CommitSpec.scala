package com.sardois.sgit

import org.scalatest._

class CommitSpec extends FlatSpec {

    "A commit" should "be doable" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val message = "Test commit"

        Test.handleException( () => {
            IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))
            IOCommit.commit(repo, repo.parent, Config(commitMessage = message))
        })

        val indexFile = IOIndex.getIndexFile(repo)
        val index = IOIndex.read(indexFile)
        val commit = Commit(message, index, Commit.rootCommitSha())
        val newCommitFile = repo/Repository.getCommitsPath()/commit.sha()

        // The format of the commit is correct
        assert(newCommitFile.contentAsString == commit.toString)

        // The index was saved
        val savedIndexedFile = IOIndex.getIndexesFolder(repo)/index.sha()
        assert(savedIndexedFile.exists)
        assert(savedIndexedFile.contentAsString == index.toString)

        // The commit sha referenced by the HEAD was updated
        val newCommitSha = IOHead.getPointedCommitSha(repo)
        assert(newCommitSha == commit.sha())

        IORepositoryTest.delete(repo)
    }
}