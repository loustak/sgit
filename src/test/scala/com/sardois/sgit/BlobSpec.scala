package com.sardois.sgit

import org.scalatest._

class BlobSpec extends FlatSpec {

    "A blob" should "be writable" in {
        val folder = Test.getRandomFolder()
        folder.createDirectories()
        val blobsFolder = folder/Repository.getBlobsPath()
        blobsFolder.createDirectories()
        val file = IOTest.createRandomFile(folder)

        IOBlob.write(blobsFolder, file) match {
            case Some(error) => fail(error)
            case _ =>
        }

        val fileShaName = blobsFolder/Util.shaFile(file)

        assert(fileShaName.exists)
        assert(fileShaName.isRegularFile)

        IOTest.delete(folder)
    }

    it should "return an error if we are trying to write a non existing file" in {
        val folder = Test.getRandomFolder().createDirectories()
        val blobsFolder = (folder/Repository.getBlobsPath()).createDirectories()
        val file = Test.getRandomFile(folder)

        IOBlob.write(blobsFolder, file) match {
            case Some(error) => succeed
            case _ => fail("No error message where provided")
        }

        IOTest.delete(folder)
    }
}