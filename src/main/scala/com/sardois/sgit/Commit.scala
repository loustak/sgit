package com.sardois.sgit

import java.util.Date

case class Commit(message: String, index: String, parentCommit: Option[Commit], author: String, date: Date) {

}

object Commit {

    def root: Commit = {
        Commit("", "", None, "", new Date())
    }
}
