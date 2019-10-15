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

    def apply(files: Iterable[(String, String)]): Iterable[IndexEntry] = {
        files.map( file => {
            IndexEntry(file)
        })
    }

    /* Create index entries without sha so it doesn't need to open
     * any files nor performs IO operations, this function is useful
     * to create index entries which have been deleted.
     */
    def fromPathsWithEmptySha(paths: Iterable[String]): Iterable[IndexEntry] = {
        paths.map( path => {
            IndexEntry(path, "")
        })
    }
}

// Create index entries with forced relative path
object IOIndexEntry {

    def fromFiles(repoFolder: File, files: Iterable[File]): Iterable[IndexEntry] = {
        files.map( file => {
            val relativePath = Repository.relativize(repoFolder, file)
            val sha = Util.shaFile(file)
            IndexEntry(relativePath, sha)
        })
    }

    def fromPaths(repoFolder: File, paths: Iterable[String]): Iterable[IndexEntry] = {
        val files = paths.map( path => File(path))
        fromFiles(repoFolder, files)
    }
}