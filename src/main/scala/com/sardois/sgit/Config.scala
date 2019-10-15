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

    interactive: Boolean = false
)
