package com.lucassardois

import better.files._
import scopt.OptionParser

object Parser {
  def apply(): OptionParser[Config] = {
    new OptionParser[Config]("sgit") {
      head("sgit", "1.0")

      cmd(name = "init")
        .action((_, c) => c.copy(mode = "init"))
        .text("Create a sgit repository in the current directory.")

      cmd(name = "status")
        .action((_, c) => c.copy(mode = "status"))
        .text("Show the working tree status.")

      cmd(name = "add")
        .action((_, c) => c.copy(mode = "add"))
        .text("Add file contents to the index.")
        .children(
          arg[String](name = "<file>...")
            .unbounded()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("list of files to add"),
        )

      cmd(name = "commit")
        .action((_, c) => c.copy(mode = "add"))
        .text("Record changes to the repository")
        .children(
          opt[String]('m', name = "message")
            .required()
            .action((x, c) => c.copy(commitMessage = x))
            .text("commit message")
        )

      cmd(name = "diff")
        .action((_, c) => c.copy(mode = "diff"))
        .text("Show changes between commits, commit and working tree, etc")

      cmd(name = "log")
        .action((_, c) => c.copy(mode = "log"))
        .text("Show all commits started with newest")
        .children(
          opt[Unit]('p', "patch")
            .action((x, c) => c.copy(patch = true))
            .text("Show changes overtime"),

          opt[Unit]('s', "stat")
            .action((x, c) => c.copy(patch = true))
            .text("Show stats about changes overtime"),
        )

      cmd(name = "tag")
        .action((_, c) => c.copy(mode = "tag"))
        .text("Add a tag reference in refs/tags/.")
        .children(
          arg[String]("<tag name>")
            .required()
            .action((x, c) => c.copy(tagName = x))
            .text("name of the tag")
        )

      cmd(name = "branch")
        .action((_, c) => c.copy(mode = "branch", showBranch = true))
        .text("Create a new branch")
        .children(
          arg[String](name = "<branch name>")
            .action((x, c) => c.copy(branchName = x))
            .optional()
            .text("name of the branch you are creating"),

          opt[Unit]('a', name = "all")
            .action((_, c) => c.copy(showBranch = true, showTag = true))
            .text("List all branches and tags"),

          opt[Unit]('v', name = "verbose")
            .action((_, c) => c.copy(verbose = true))
        )

      cmd(name = "checkout")
        .action((_, c) => c.copy(mode = "checkout"))
        .text("move the HEAD to the branch, tag or commit hash")
        .children(
          arg[String](name = "<branch or tag or commit hash>")
            .action((x, c) => c.copy(branchOrCommit = x))
            .text("branch or tag or commit hash")
            .required()
        )

      cmd(name = "merge")
        .action((_, c) => c.copy(mode = "merge"))
        .text("Join two development histories together")
        .children(
          arg[String](name = "<branch>")
            .action((x, c) => c.copy(branchName = x))
            .text("the branch to merge to the current one")
        )

      cmd(name = "rebase")
        .action((_, c) => c.copy(mode = "rebase"))
        .text("Reapply commits on top of another base tip")
        .children(
          arg[String](name = "<branch or commit>")
            .action((x, c) => c.copy(branchOrCommit = x))
            .text("branch/commit that will be the new base of the current branch"),

          opt[Unit]('i', name = "interactive")
            .action((_, c) => c.copy(interactive = true))
            .text("interfaces in order to rename, squash and do random stuff on commits before rebasing")
        )
    }
  }
}