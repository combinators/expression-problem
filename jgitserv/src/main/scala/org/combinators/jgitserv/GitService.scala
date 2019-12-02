package org.combinators.jgitserv

import java.io.FileInputStream
import java.nio.file.{Files, Paths}

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import com.twitter.finagle.{Http, ListeningServer}
import com.twitter.io.{Buf, Reader}
import com.twitter.util.Future
import io.finch._
import io.finch.Encode._
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.Git

import scala.jdk.CollectionConverters._

/** A simple endpoint to host Git repositories.
  *
  * Uses the [[https://git-scm.com/book/en/v2/Git-Internals-Transfer-Protocols "dumb" protocol]],
  * which means repositories cannot be written to externally.
  * The repository is stored in the system's temporary directory and its contents will be lost when the server stops.
  *
  * @constructor Create a new git repository.
  *
  * @see This [[https://examples.javacodegeeks.com/core-java/io/java-io-tmpdir-example/ tutorial]] on
  *      how to set the temporary directory for repositories.
  */
class GitService(transactions: Seq[BranchTransaction], repositoryName: String = "", port: Int = 8081) extends IOApp {
  /** The Git repository in which files are stored while the server is active. */
  private lazy val git: Resource[IO, Git] = {
    def acquire: IO[Git] = {
      for {
        git <- IO {
          val directory = Files.createTempDirectory("jgitserv_hosted_git")
          Git.init().setDirectory(directory.toFile).call()
        }
        _ <- transactions.map(t => t.materialize(git)).toList.sequence
      } yield git
    }
    def release(git: Git): IO[Unit] = IO {
      val directory = git.getRepository.getWorkTree
      git.close()
      try {
        FileUtils.deleteDirectory(directory)
      } catch {
        case io:java.io.IOException => println ("Couldn't delete:" + io.getMessage)
      }
    }
    Resource.make(acquire)(release)
  }

  class Endpoints(repo: Git) extends Endpoint.Module[IO] {
    val dumbProtocol: Endpoint[IO, String] =
      get("info" :: "refs") {
        IO {
          Ok(repo
            .branchList()
            .call()
            .asScala
            .map (ref => s"${ref.getObjectId.getName}\t${ref.getName}")
            .mkString("", "\n", "\n"))
        }
      }

    val content: Endpoint[IO, Buf] =
      get(paths[String]) { pathSegments: List[String] =>
        Reader.fromStream(
          new FileInputStream(Paths.get(repo.getRepository.getDirectory.toString, pathSegments: _*).toFile)
        ).read().map(res => Ok(res.get))
      } handle {
        case fnf: java.io.FileNotFoundException => NotFound(fnf)
        case ex: Exception => BadRequest(ex)
      }

    val gitEndpoint = {
      if (!repositoryName.isEmpty) {
        get(repositoryName) :: (dumbProtocol :+: content)
      } else {
        (dumbProtocol :+: content)
      }
    }

    def serve: IO[ListeningServer] = IO(
      Http.server
        .withStreaming(enabled = true)
        .serve(":8081",
          Bootstrap.configure().serve[Text.Plain](gitEndpoint).toService
        )
    )
  }

  def runProcess(cmd: String): IO[ExitCode] = {
    var result = 0

    git.use(git => {
      val server = Resource.make(new Endpoints(git).serve){ s =>
        IO.suspend(implicitly[ToAsync[Future, IO]].apply(s.close()))
      }
      server
        .use(_ => IO {
          import sys.process._
          val dir = git.getRepository.getDirectory.getParentFile

          println("working in:" + dir)
          val batFile = "run.bat"

          val out = new StringBuilder
          val err = new StringBuilder

          val logger = ProcessLogger(
            (o: String) => out.append(o).append("\n"),
            (e: String) => err.append(e).append("\n"))

          try {
            val pw = new java.io.PrintWriter(new java.io.FileWriter(new java.io.File(dir, batFile)))
            pw.println(cmd)
            pw.println("EXIT /B %errorlevel%") // HACK for windows
            pw.close()

            result = Process(dir.getAbsolutePath + java.io.File.separator + batFile, git.getRepository.getDirectory.getParentFile)! logger

            System.out.println("Deleting files...")
            System.out.println(out)
            System.err.println(err)
          } catch {
            case e: Exception =>
              println ("Unexpected error:" + e.getMessage)
          }
        })
        .as(ExitCode(result))
    })
  }

  def run(args: List[String]): IO[ExitCode] = {
    git.use(git => {
      val server = Resource.make(new Endpoints(git).serve){ s =>
        IO.suspend(implicitly[ToAsync[Future, IO]].apply(s.close()))
      }
      server
        .use(_ => IO {
          System.out.println("Press enter to stop the server")
          scala.io.StdIn.readLine()
        })
        .as(ExitCode.Success)
    })
  }
}
