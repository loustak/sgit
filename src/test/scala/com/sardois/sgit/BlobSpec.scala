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

        IOBlob.write(blobsFolder, file)

        val fileShaName = blobsFolder/Util.shaFile(file)

        assert(fileShaName.exists)
        assert(fileShaName.isRegularFile)

        IOTest.delete(folder)
    }

    it should "not be created if it as the same sha as another blob" in {
        val folder = IOTest.createRandomFolder()
        val blobsFolder = createBlobsDir(folder)
        val file = IOTest.createRandomFile(folder)

        IOBlob.write(blobsFolder, file)
        IOBlob.write(blobsFolder, file)

        assert(blobsFolder.listRecursively.size == 1)

        IOTest.delete(folder)
    }
}