package com.sardois.sgit

import org.scalatest._

class RepositoryIndexSpec extends FlatSpec {

    "A repository index" should "be able to add files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val files = List(file.pathAsString)

        IOIndex.add(repo, repo.parent, Config(paths = files))

        val index = repo/Repository.getIndexPath()
        if (!index.exists) {
            fail("The index doesn't exists")
        }

        val lines = index.lines().toList
        assert(lines.size == files.size)

        val entry = lines(0)
        val split = entry.split(" ")
        if (split.size < 2) {
            fail("Invalid entry in the index")
        }

        val path = split(0)
        val sha = split(1)

        assert(path == Repository.relativePathFromRepo(repo, file))
        assert(sha == Util.shaFile(file))

        IORepositoryTest.delete(repo)
    }

    it should "be able to add nested files" in {
        val repo = IORepositoryTest.init()
        val nested = (repo.parent/"nested").createDirectories()
        val veryNested = (nested/"nestedAgain").createDirectories()
        IOTest.createRandomFile(veryNested)

        IOIndex.add(repo, repo.parent, Config(paths = List(nested.pathAsString)))

        val indexFile = IOIndex.getIndexFile(repo)

        assert(indexFile.lines.size == 1)

        IORepositoryTest.delete(repo)
    }

    it should "write added files as blobs" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)

        IOIndex.add(repo, repo.parent, Config(paths = List(file.pathAsString)))

        IORepositoryTest.delete(repo)
    }

    it should "not write two identical files as two blobs" in {
        // But, it should still write two index entry pointing to only one blob
        val repo = IORepositoryTest.init()
        val originalFile = IOTest.createRandomFile(repo.parent)
        // Create a copy of the previous file (to get the same sha)
        val copiedFile = (repo.parent/"copy").write(originalFile.contentAsString)

        if (Util.shaFile(copiedFile) != Util.shaFile(originalFile)) {
            fail("The copied file have a different sha than the original")
        }

        val fileList = List(originalFile.pathAsString, copiedFile.pathAsString)

        IOIndex.add(repo, repo.parent, Config(paths = fileList))

        // The number of index entry should be two
        val indexFile = IOIndex.getIndexFile(repo)
        assert(indexFile.lineCount == 2)

        // Only one blob should had been created since
        // the two added files have the same sha
        val blobFolder = IOBlob.getBlobsFolder(repo)
        assert(blobFolder.list.size == 1)

        IORepositoryTest.delete(repo)
    }

    it should "be readable" in {
        val repo = IORepositoryTest.init()
        val dir = repo.parent
        val files = List(
            IOTest.createRandomFile(dir),
            IOTest.createRandomFile(dir),
            IOTest.createRandomFile(dir)
        )

        val filesPath = Util.filesToPath(files)
        IOIndex.add(repo, repo.parent, Config(paths = filesPath))

        val indexFile = IOIndex.getIndexFile(repo)
        val index = IOIndex.read(indexFile)

        assert(indexFile.isRegularFile)
        assert(index.size == files.size)

        IORepositoryTest.delete(repo)
    }

    it should "be able to list untracked files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val files = List(file.pathAsString)

        val indexFile = IOIndex.getIndexFile(repo)
        val index = IOIndex.read(indexFile)
        val untrackedFiles = IOIndex.getUntrackedFiles(repo, index, files)

        assert(untrackedFiles.size == files.size)

        IORepositoryTest.delete(repo)
    }

    it should "be able to list untracked nested files" in {
        val repo = IORepositoryTest.init()
        val nested = (repo.parent/"nested").createDirectories()
        val file1 = IOTest.createRandomFile(nested)
        val nestedAgain = (nested/"nestedAgain").createDirectories()
        val file2 = IOTest.createRandomFile(nestedAgain)

        val files = List(file1.pathAsString, file2.pathAsString)

        val indexFile = IOIndex.getIndexFile(repo)
        val index = IOIndex.read(indexFile)
        val untrackedFiles = IOIndex.getUntrackedFiles(repo, index, files)

        assert(untrackedFiles.size == files.size)

        IORepositoryTest.delete(repo)
    }

    it should "be able to list not staged modified files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val files = List(file.pathAsString)

        // First be sure to track the file inside the repo
        IOIndex.add(repo, repo.parent, Config(paths = files))
        IOTest.modifyRandomFile(file)

        val indexFile = IOIndex.getIndexFile(repo)
        val index = IOIndex.read(indexFile)
        val modifiedFiles = IOIndex.getStatusNotStagedModifiedFiles(repo, index, files)

        assert(modifiedFiles.size == 1)

        IORepositoryTest.delete(repo)
    }

    it should "be able to list not staged deleted files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val files = List(file.pathAsString)

        IOIndex.add(repo, repo.parent, Config(paths = files))
        file.delete()

        val indexFile = IOIndex.getIndexFile(repo)
        val index = IOIndex.read(indexFile)
        val deletedFiles = IOIndex.getStatusNotStagedDeletedFiles(index, files)
        assert(deletedFiles.size == 1)

        IORepositoryTest.delete(repo)
    }

    it should "be able to list staged modified files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val files = List(file.pathAsString)

        IOIndex.add(repo, repo.parent, Config(paths = files))
        IOCommit.commit(repo, repo.parent, Config(commitMessage =  "Test"))
        IOTest.modifyRandomFile(file)
        IOIndex.add(repo, repo.parent, Config(paths = files))

        val newIndex = IOIndex.getIndex(repo)
        val oldIndex = IOHead.getPointedIndex(repo)
        val modifiedFiles = IOIndex.getStatusStagedModifiedFiles(newIndex, oldIndex)

        assert(modifiedFiles.size == files.size)

        IORepositoryTest.delete(repo)
    }

    it should "be able to list staged deleted files" in {
        val repo = IORepositoryTest.init()
        val file = IOTest.createRandomFile(repo.parent)
        val files = List(file.pathAsString)

        IOIndex.add(repo, repo.parent, Config(paths = files))
        IOCommit.commit(repo, repo.parent, Config(commitMessage =  "Test"))
        IOIndex.remove(repo, repo.parent, Config(paths = files))

        val newIndex = IOIndex.getIndex(repo)
        val oldIndex = IOHead.getPointedIndex(repo)
        val deletedFiles = IOIndex.getStatusStagedDeletedFiles(newIndex, oldIndex)

        assert(deletedFiles.size == files.size)
    }
}