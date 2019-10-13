package com.sardois.sgit

import better.files.File

class IndexEntry(val relativePath: String, val sha: String) {

}

object IndexEntry {

    def apply(relativePath: String, sha: String): IndexEntry = {
        new IndexEntry(relativePath, sha)
    }

    def apply(file: (String, String)): IndexEntry = {
        new IndexEntry(file._1, file._2)
    }

    def apply(files: List[(String, String)]): List[IndexEntry] = {
        files.map( file => {
            IndexEntry(file)
        })
    }

    /* Create index entries without sha so it doesn't need to open
     * any files nor performs IO operations, this function is useful
     * to create index entries which have been deleted.
     */
    def fromPathsWithEmptySha(paths: List[String]): List[IndexEntry] = {
        paths.map( path => {
            IndexEntry(path, "")
        })
    }
}

// Create index entries with forced relative path
object IOIndexEntry {

    def fromFiles(repoFolder: File, files: List[File]): List[IndexEntry] = {
        files.map( file => {
            val relativePath = Repository.relativePathFromRepo(repoFolder, file)
            val sha = Util.shaFile(file)
            IndexEntry(relativePath, sha)
        })
    }

    def fromPaths(repoFolder: File, paths: List[String]): List[IndexEntry] = {
        val files = paths.map( path => File(path))
        fromFiles(repoFolder, files)
    }
}