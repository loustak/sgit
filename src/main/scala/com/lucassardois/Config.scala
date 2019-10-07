package com.lucassardois

import better.files._

case class Config(
    mode: String = "",
    commitMessage: String = "",
    files: Array[String] = Array(),
    patch: Boolean = false,
    stat: Boolean = false,
    tagName: String = "",
    verbose: Boolean = false,
    showBranch: Boolean = false,
    branchName: String = "",
    branchOrCommit: String = "",
    showTag: Boolean = false,
    file: File = File("."),
    interactive: Boolean = false
)