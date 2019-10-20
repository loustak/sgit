package com.sardois.sgit

import com.sardois.sgit.Diff.Diffs

object UI {

    val newline: String = System.lineSeparator()

    def commit(newCommit: Commit): String = {
        "Now on commit: " + newCommit.sha + "."
    }

    def status(
                  head: Head,
                  newStagedFiles: List[String],
                  modifiedStagedFiles: List[String],
                  deletedStagedFiles: List[String],
                  modifiedNotStagedFiles: List[String],
                  deletedNotStagedFiles: List[String],
                  untrackedFiles: List[String]
              ): String = {

        // Branch, tag & commit
        val checkableStr = if (head.isBranch) {
            "On branch " + head.branchTagOrCommitSha + "."
        } else if (head.isTag) {
            "On tag " + head.branchTagOrCommitSha + "."
        } else {
            "On commit " + head.branchTagOrCommitSha + "."
        }

        // Staged changes
        val newStagedStr = if (newStagedFiles.isEmpty) "" else {
            newStagedFiles.map(str => "added: " + str + newline).mkString
        }
        val modifiedStagedStr = if (modifiedStagedFiles.isEmpty) "" else {
            modifiedStagedFiles.map(str => "modified: " + str + newline).mkString
        }
        val deletedStagedStr = if (deletedStagedFiles.isEmpty) "" else {
            deletedStagedFiles.map(str => "deleted: " + str + newline).mkString
        }
        val stagedStr = List(newStagedStr, modifiedStagedStr, deletedStagedStr).mkString
        val stagedStrTitle = if (stagedStr == "") "" else newline + "Staged changes:" + newline

        // Not staged changes
        val modifiedNotStagedStr = if (modifiedNotStagedFiles.isEmpty) "" else {
            modifiedNotStagedFiles.map(str => "modified: " + str + newline).mkString
        }

        val deletedNotStagedStr = if (deletedNotStagedFiles.isEmpty) "" else {
            deletedNotStagedFiles.map(str => "deleted: " + str + newline).mkString
        }
        val notStagedStr = List(modifiedNotStagedStr, deletedNotStagedStr).mkString
        val notStagedTitle = if (notStagedStr == "") "" else newline + "Not staged changes:" + newline

        // Untracked files
        val untrackedStr = if (untrackedFiles.isEmpty) "" else {
            untrackedFiles.map(str => "untracked: " + str + newline).mkString
        }
        val untrackedTitle = if (untrackedStr == "") "" else newline + "Untracked files:" + newline

        checkableStr + newline + stagedStrTitle + stagedStr + notStagedTitle +
            notStagedStr + untrackedTitle + untrackedStr
    }

    def branchAndTags(
                         head: Head,
                         branches: List[Branch],
                         tags: List[Tag]
                     ): String = {

        val branchTitle = "Branches:" + newline
        val branchStr = if (branches.isEmpty) "" else {
            branches.map(b => {
                val curr = if (b.name == head.branchTagOrCommitSha) "* " else ""
                curr + b.name + " " + b.commitSha + newline
            }).mkString
        }

        val tagTitle = "Tags:" + newline
        val tagStr = if (tags.isEmpty) "" else {
            tags.map(t => {
                val curr = if (t.name == head.branchTagOrCommitSha) "* " else ""
                curr + t.name + " " + t.commitSha + newline
            }).mkString
        }

        branchTitle + branchStr + newline + tagTitle + tagStr
    }

    def logCommit(commit: Commit): String = {
        "commit  " + commit.sha + newline +
            "Author: " + commit.author + newline +
            "Date:   " + commit.dateString + newline + newline +
            "      "   + commit.message + newline + newline
    }

    def log(commits: List[Commit]): String = {
        commits.map( commit => logCommit(commit)).mkString
    }

    def logDiff(commit1: Commit, commit2: Commit, changes: Map[String, Diffs]): String = {
        val commitStr = "Diff for commit:" + newline +
            "    " + commit1.sha + newline +
            "    " + commit2.sha + newline + newline

        val changesStr = changes.keys.map(path => {
            val diffs = changes(path)
            if (diffs.isEmpty) "" else {
                val (insertions, deletions) = diffs.partition(diff => {
                    diff.diffEnum == DiffEnum.ADD
                })
                path + ": " + newline +
                    deletions.map(del => {
                        val realLineNumber = del.lineNumber + 1
                        realLineNumber.toString + "|- " + del.line
                    }).mkString(newline) +
                    insertions.map(insert => {
                        val realLineNumber = insert.lineNumber + 1
                        realLineNumber.toString + "|+ " + insert.line
                    }).mkString(newline)
            }
        }).mkString

        commitStr + changesStr
    }

    def logStats(commit: Commit, changes: Map[String, Diffs]): String = {
        val commitStr = logCommit(commit)

        val changesStr = changes.keys.map(path => {
            val diffs = changes(path)
            if (diffs.isEmpty) "" else {
                val (insertions, deletions) = diffs.partition( diff => {
                    diff.diffEnum == DiffEnum.ADD
                })
                path + ": " + insertions.length + " insertions, " + deletions.length + " deletions" + newline
            }
        }).mkString

        commitStr + changesStr + newline
    }

    def logPatch(commit: Commit, changes: Map[String, Diffs]): String = {
        val commitStr = logCommit(commit)

        val changesStr = changes.keys.map(path => {
            val diffs = changes(path)
            if (diffs.isEmpty) "" else {
                val (insertions, deletions) = diffs.partition(diff => {
                    diff.diffEnum == DiffEnum.ADD
                })
                path + ": " + newline +
                    deletions.map(del => {
                        val realLineNumber = del.lineNumber + 1
                        realLineNumber.toString + "|- " + del.line
                    }).mkString(newline) +
                    insertions.map(insert => {
                        val realLineNumber = insert.lineNumber + 1
                        realLineNumber.toString + "|+ " + insert.line
                    }).mkString(newline)
            }
        }).mkString

        commitStr + changesStr + newline + newline
    }

    @impure
    def printSuccess(str: String): Unit = {
        print(str)
    }

    @impure
    def printError(str: String): Unit = {
        print(str)
    }
}
