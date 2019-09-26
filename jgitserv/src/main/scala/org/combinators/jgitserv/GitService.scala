package org.combinators.jgitserv

import java.io.{FileInputStream, InputStream}
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ContextShift, IO, Resource, Sync}
import com.twitter.finagle.Http
import com.twitter.io.Buf
import com.twitter.util.Await
import io.finch._
import io.finch.endpoint._
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.Git
import shapeless.HNil

/** A simple endpoint to host Git repositories.
  *
  * Uses the [[https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols "dumb" protocol]],
  * which means repositories cannot be written to externally.
  * The repository is stored in the system's temporary directory and its contents will be lost when the server stops.
  *
  * @constructor Create a new git repository, which stores its contents relative to `sourceDirectory`.
  * @param sourceDirectory The path (relative to the Git root) where to store solutions.
  *
  * @see This [[https://examples.javacodegeeks.com/core-java/io/java-io-tmpdir-example/ tutorial]] on
  *      how to set the temporary directory for repositories.
  */
class GitService(
  val sourceDirectory: Path = Paths.get("."),
  transactions: Seq[BranchTransaction]
) extends Endpoint.Module[IO] {
  /** The Git repository in which files are stored while the server is active. */
  private lazy val git: Resource[IO, Git] = {
    def acquire: IO[Git] = IO {
      val directory = Files.createTempDirectory("jgitserv_hosted_git")
      Git.init().setDirectory(directory.toFile).call()
    }
    def release(git: Git): IO[Unit] = IO {
      val directory = git.getRepository.getWorkTree
      git.close()
      FileUtils.deleteDirectory(directory)
    }
    Resource.make(acquire)(release)
  }

  /*def content(implicit F: Sync[IO], S: ContextShift[IO]): Endpoint[IO, Buf] = get(paths[String]) { (segments: List[String]) =>
    fromInputStream(git.map(repo =>
      new FileInputStream(Paths.get(repo.getRepository.getDirectory.toPath.toString, segments:_*).toFile)
    )).map( buf =>
      Ok(buf)
    )
  }*/

/*

  /** The temporary Git file structure. */
  private lazy val git = Git.init().setDirectory(root.toFile).call()
  /** A mutable collection to store which inhabitants are already serialized. */
  private lazy val computedVariations = collection.mutable.Set.empty[Long]

  /** All combinator names together with their reflected type information. */
  val combinatorComponents: Map[String, CombinatorInfo]

  /** The path (relative to the Git root) where to store solutions. */
  val sourceDirectory: Path = Paths.get(".")

  /** The computed result location (root/sourceDirectory) */
  implicit final val resultLocation: ResultLocation = ResultLocation(root.resolve(sourceDirectory))

  /** The results to present. */
  val results: Results

  /** Creates a new variation branch. */
  private def checkoutEmptyBranch(id: Long): Unit = {
    git
      .checkout()
      .setOrphan(true)
      .setName(s"variation_$id")
      .call()
    git.reset()
      .setMode(ResetType.HARD)
      .call()
  }

  /** Commits all files to the current branch */
  private def addAllFilesToCurrentBranch(): RevCommit = {
    git
      .add()
      .addFilepattern(".")
      .call()
    git
      .commit()
      .setMessage("Next variation")
      .call()
  }

  /** Creates the dumb protocol [[https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols]] file.
    *
    * @param rev branch head to create the file for.
    * @param id  variation number of the result.
    */
  private def updateInfo(rev: RevCommit, id: Long): Unit = {
    val info = Paths.get(root.toString, ".git", "info")
    Files.createDirectories(info)
    val refs = Paths.get(root.toString, ".git", "info", "refs")
    val line = s"${rev.getName}\trefs/heads/variation_$id\n"
    Files.write(refs, line.getBytes, StandardOpenOption.APPEND, StandardOpenOption.CREATE)
  }

  /** Creates a Git containing the `number`-th result.
    *
    * @param number index of the result to store.
    * @return term representation of the uninterpreted result.
    */
  def prepare(number: Long) = Action {
    val branchFile = Paths.get(root.toString, ".git", "refs", "heads", s"variation_$number")
    val result = results.raw.index(number).toString
    if (!Files.exists(branchFile)) {
      checkoutEmptyBranch(number)
      results.storeToDisk(number)
      val rev = addAllFilesToCurrentBranch()
      updateInfo(rev, number)
      computedVariations.add(number)
    }
    Ok(result)
  }

  /** Renders an overview page with access to all inhabitation results.
    *
    * @return the html code of the page.
    */
  def overview() = Action { request =>
    val combinators = combinatorComponents.mapValues {
      case staticInfo: StaticCombinatorInfo =>
        (ReflectedRepository.fullTypeOf(staticInfo),
          s"${scala.reflect.runtime.universe.show(staticInfo.fullSignature)}")
      case dynamicInfo: DynamicCombinatorInfo[_] =>
        (ReflectedRepository.fullTypeOf(dynamicInfo),
          dynamicInfo.position.mkString("\n"))
    }
    Ok(html.overview.render(
      request.path,
      webJars,
      combinators,
      results.targets,
      results.raw,
      computedVariations.toSet,
      results.infinite,
      results.incomplete))
  }

  /** Returns the uninterpreted raw representation of the `number`-th inhabitant. */
  def raw(number: Long) = Action {
    try {
      Ok(results.raw.index(number).toString())
    } catch {
      case _: IndexOutOfBoundsException => play.api.mvc.Results.NotFound(s"404, Inhabitant not found: $number")
    }
  }

  /** Serves a file from the Git of all inhabitants.
    *
    * @param name file name relative to the Git root.
    * @return the file contents as an array of bytes.
    */
  def serveFile(name: String) = Action {
    try {
      Ok(Files.readAllBytes(root.resolve(Paths.get(".git", name))))
    } catch {
      case _: NoSuchFileException => play.api.mvc.Results.NotFound(s"404, File not found: $name")
      case _: AccessDeniedException => play.api.mvc.Results.Forbidden(s"403, Forbidden: $name")
    }
  }
   */
}
