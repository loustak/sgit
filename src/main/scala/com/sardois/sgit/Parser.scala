package com.sardois.sgit

import scopt.OptionParser

object Parser {
    def apply(): OptionParser[Config] = {
        new OptionParser[Config]("sgit") {
            head("sgit", "1.0")
            help("help")

            cmd(name = "init")
                .action((_, c) => c.copy(mode = "init"))
                .text("create a sgit repository in the current directory.")

            cmd(name = "add")
                .action((_, c) => c.copy(mode = "add"))
                .text("add file contents to the index.")
                .children(
                    arg[String](name = "<file>...")
                        .unbounded()
                        .action((x, c) => c.copy(paths = c.paths :+ x))
                        .text("list of files to add"),
                )

            cmd(name = "commit")
                .action((_, c) => c.copy(mode = "commit"))
                .text("record changes to the repository.")
                .children(
                    opt[String]('m', name = "message")
                        .required()
                        .action((x, c) => c.copy(commitMessage = x))
                        .text("commit message")
                )

            cmd(name = "status")
                .action((_, c) => c.copy(mode = "status"))
                .text("show the working tree status.")

            cmd(name = "branch")
                .action((_, c) => c.copy(mode = "branch"))
                .text("branch management.")
                .children(
                    arg[String](name = "<branch name>")
                        .action((x, c) => c.copy(branchName = x))
                        .text("create a branch with the given name.")
                        .optional(),

                    opt[Unit](name = "all verbose")
                        .abbr("av")
                        .action((_, c) => c.copy(list = true))
                        .text("list all branches and tags."),

                    checkConfig( c =>
                       if (c.mode == "branch" && c.branchName  == "" && !c.list) {
                           failure("missing branch name.")
                       }
                       else success
                    )
                )

            cmd(name = "tag")
                .action((_, c) => c.copy(mode = "tag"))
                .text("Add a tag reference.")
                .children(
                    arg[String]("<tag name>")
                        .action((x, c) => c.copy(tagName = x))
                        .text("name of the tag.")
                        .required()
                )

            cmd(name = "checkout")
                .action((_, c) => c.copy(mode = "checkout"))
                .text("move the HEAD to the branch, tag or commit hash")
                .children(
                    arg[String](name = "<branch, tag or commit hash>")
                        .action((x, c) => c.copy(branchTagOrCommit = x))
                        .text("branch, tag or commit hash")
                        .required()
                )

            cmd(name = "diff")
                .action((_, c) => c.copy(mode = "diff"))
                .text("Show changes between commits, commit and working tree, etc...")
                .children(
                    arg[String](name = "<commit1 hash>")
                        .action((x, c) => c.copy(commit1 = x))
                        .text("the first commit")
                        .required(),
                    arg[String](name = "<commit2 hash>")
                        .action((x, c) => c.copy(commit2 = x))
                        .text("the second commit")
                        .required()
                )

            cmd(name = "log")
                .action((_, c) => c.copy(mode = "log"))
                .text("Show all commits started with newest.")
                .children(
                    opt[Unit]('p', "patch")
                        .action((_, c) => c.copy(patch = true))
                        .text("Show changes overtime"),

                    opt[Unit]('s', "stat")
                        .action((_, c) => c.copy(stat = true))
                        .text("Show stats about changes overtime."),
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
