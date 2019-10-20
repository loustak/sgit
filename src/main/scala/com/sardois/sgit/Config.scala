package com.sardois.sgit

case class Config(
    mode: String = "",

    commitMessage: String = "",

    paths: Array[String] = Array(),

    patch: Boolean = false,
    stat: Boolean = false,
    verbose: Boolean = false,

    branchName: String = "",
    list: Boolean = false,
    tagName: String = "",

    branchOrCommit: String = "",
    branchTagOrCommit: String = "",

    commit1: String = "",
    commit2: String = "",

    interactive: Boolean = false
)
