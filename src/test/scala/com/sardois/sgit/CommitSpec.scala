package com.sardois.sgit

import org.scalatest._

class CommitSpec extends FlatSpec {

    "A commit" should "be writable" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val message = "Test commit"

        Util.handleException(() => {
            IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))
            IOCommit.commit(repo, repo.parent, Config(commitMessage = message))
        }) match {
            case Some(value) => fail(value)
            case None =>
        }

        val indexFile = IOIndex.getIndexFile(repo)
        val index = IOIndex.read(indexFile)
        val commit = Commit(message, index)
        val newCommitFile = repo/Repository.getCommitsPath()/commit.sha()
        assert(newCommitFile.contentAsString == commit.toString)

        IORepositoryTest.delete(repo)
    }
}