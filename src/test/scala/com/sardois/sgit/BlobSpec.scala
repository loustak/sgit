package com.sardois.sgit


import better.files.File
import org.scalatest._

class BlobSpec extends FlatSpec {

    def createBlobsDir(folder: File): File = {
        (folder/Repository.getBlobsPath()).createDirectories()
    }

    "A blob" should "be writable" in {
        val folder = IOTest.createRandomFolder()
        val blobsFolder = createBlobsDir(folder)
        val file = IOTest.createRandomFile(folder)

        Test.handleException( () => {
            IOBlob.write(blobsFolder, file)
            None
        })

        val fileShaName = blobsFolder/Util.shaFile(file)

        assert(fileShaName.exists)
        assert(fileShaName.isRegularFile)

        IOTest.delete(folder)
    }

    it should "return an error if we are trying to write a non existing file" in {
        val folder = IOTest.createRandomFolder()
        val blobsFolder = createBlobsDir(folder)
        val file = Test.getRandomFile(folder)

        Util.handleException(() => {
            IOBlob.write(blobsFolder, file)
            None
        }) match {
            case Some(value) => succeed
            case None => fail("No error message provided")
        }

        IOTest.delete(folder)
    }

    it should "not be created if it as the same sha as another blob" in {
        val folder = IOTest.createRandomFolder()
        val blobsFolder = createBlobsDir(folder)
        val file = IOTest.createRandomFile(folder)

        Util.handleException( () => {
            IOBlob.write(blobsFolder, file)
            IOBlob.write(blobsFolder, file)
            None
        })

        assert(blobsFolder.listRecursively.size == 1)

        IOTest.delete(folder)
    }
}