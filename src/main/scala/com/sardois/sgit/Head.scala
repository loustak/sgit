package com.sardois.sgit

import better.files._

object Head {

}

object IOHead {

    def getHeadFile(repoFolder: File): File = {
        repoFolder/Repository.getHeadPath()
    }

    def write(repoFolder: File, checkable: Checkable): Unit = {
        val headFile = getHeadFile(repoFolder)
        
    }
}