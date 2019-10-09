package com.lucassardois

import better.files._

case class Config(
    mode: String = "",
    commitMessage: String = "",
    paths: List[String] = Nil,
    patch: Boolean = false,
    stat: Boolean = false,
    tagName: String = "",
    verbose: Boolean = false,
    showBranch: Boolean = false,
    branchName: String = "",
    branchOrCommit: String = "",
    showTag: Boolean = false,
    interactive: Boolean = false
)