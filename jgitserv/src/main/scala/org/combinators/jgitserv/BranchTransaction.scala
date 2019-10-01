package org.combinators.jgitserv

import java.io.File

import scala.collection.JavaConverters._
import java.nio.file.{Path, Paths}

import cats.effect.IO
import cats.implicits._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.FileFilterUtils
import org.combinators.templating.persistable.Persistable
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.ResetCommand.ResetType
import org.eclipse.jgit.util.{FileUtils => JGitFileUtils}


/** Bundles multiple Git commands into a sequence of actions,
  * which are later materialized in a given git repository.
  *
  * @see [[org.combinators.jgitserv.BranchTransaction$]] on ways to start a transaction.
  */
sealed trait BranchTransaction { self =>
  /** The name of the branch. */
  val branchName: String
  /** Materializes this branch transaction in a Git. */
  def materialize(inGit: Git): IO[Unit]

  /** Forks this branch into a new branch. */
  def fork(newName: String): BranchTransaction = new BranchTransaction {
    /** The name of the branch */
    val branchName: String = newName
    /** Materializes this branch in a Git. */
    def materialize(inGit: Git): IO[Unit] =
      self.materialize(inGit) *> IO {
        inGit.checkout()
          .setCreateBranch(true)
          .setName(branchName)
          .setStartPoint(s"refs/heads/${self.branchName}")
          .setForced(true)
          .call()
      }
  }

  /** Deletes all files in this branch and updates the index accordingly. */
  def deleteAllFiles: BranchTransaction = new BranchTransaction {
    val branchName = self.branchName
    def materialize(inGit: Git): IO[Unit] =
      self.materialize(inGit) *> IO {
        val tree = JGitFileUtils.pathToString(JGitFileUtils.canonicalize(inGit.getRepository.getWorkTree))
        val toDelete: Iterator[(File, String)] =
          FileUtils
            .iterateFilesAndDirs(
              inGit.getRepository.getWorkTree,
              FileFilterUtils.trueFileFilter(),
              FileFilterUtils.trueFileFilter()
              )
            .asScala
            .filterNot(f =>
              f == inGit.getRepository.getWorkTree ||
              f.toPath.startsWith(inGit.getRepository.getDirectory.toPath))
            .map(f => {
              val canonicalF = JGitFileUtils.pathToString(JGitFileUtils.canonicalize(f))
              val pat = JGitFileUtils.relativizeGitPath(tree, canonicalF)
              (f, pat)
            })
        toDelete.foreach { case (f, pat) =>
          if (!f.isDirectory) {
            inGit.rm().addFilepattern(pat).call()
          }
        }
      }
  }

  /** Persists the given element to disk and adds it to the index, overwrites existing elements.
    * Elements are stored relative to the given source directory, which is the Git root directory by default.
    */
  def persist[E](elem: E, sourceDirectory: Path = Paths.get("."))(implicit persistable: Persistable.Aux[E]): BranchTransaction = new BranchTransaction {
    val branchName = self.branchName
    def materialize(inGit: Git): IO[Unit] =
      self.materialize(inGit) *> IO {
        val tree = inGit.getRepository.getWorkTree.toPath
        val treeString = JGitFileUtils.pathToString(JGitFileUtils.canonicalize(tree.toFile))
        val persisted = persistable.persistOverwriting(tree.resolve(sourceDirectory), elem)
        val targetPath = tree.resolve(persisted.toPath)
        val targetString = JGitFileUtils.pathToString(JGitFileUtils.canonicalize(targetPath.toFile))
        val pat = JGitFileUtils.relativizeGitPath(treeString, targetString)
        inGit.add().addFilepattern(pat).call()
      }
  }

  /** Persists the given elements to disk and adds them to the index, overwrites existing elements. */
  def persist[E](elems: Seq[E])(implicit persistable: Persistable.Aux[E]): BranchTransaction =
    elems.foldLeft(this)((branch, elem) => branch.persist(elem))

  /** Commits the current index state to the branch. */
  def commit(message: String = ""): BranchTransaction = new BranchTransaction {
    val branchName = self.branchName
    def materialize(inGit: Git): IO[Unit] =
      self.materialize(inGit) *> IO {
        inGit.commit().setMessage(message).call()
      }
  }
}

/** Provides entry points to start a new [[org.combinators.jgitserv.BranchTransaction]] */
object BranchTransaction {
  /** Creates an empty orphan Git branch */
  def empty(name: String): BranchTransaction = new BranchTransaction {
    val branchName = name
    def materialize(inGit: Git): IO[Unit] = IO {
      inGit
        .checkout()
        .setOrphan(true)
        .setName(name)
        .call()
      inGit.reset()
        .setMode(ResetType.HARD)
        .call()
    }
  }

  /** Checks out an existing branch. */
  def checkout(branch: String): BranchTransaction = new BranchTransaction {
    val branchName: String = branch
    def materialize(inGit: Git): IO[Unit] = IO {
      inGit.checkout()
        .setName(branchName)
        .setForced(true)
        .call()
    }
  }
}