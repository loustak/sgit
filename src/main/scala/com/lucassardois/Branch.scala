package com.lucassardois

object Branch {
    

    def getFilePath() = {
    }
}

class Branch(val name: String, val commit: Commit) {

    def isMaster(): Boolean = name == "master"

    def write() = {
        
    }
}