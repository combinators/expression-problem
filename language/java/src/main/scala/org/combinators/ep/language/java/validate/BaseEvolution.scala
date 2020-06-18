package org.combinators.ep.language.java.validate     /*DD:LD:AI*/

import cats.effect.{ExitCode, IO}
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.language.java.{CodeGenerator, PartiallyBoxed}
import org.combinators.jgitserv.{BranchTransaction, GitService}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
trait BaseEvolution {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  // all known branches in largest initial case study.
  val evolutions = Seq(M0, M1, M2, M3, M4, M5, M6, M7, I1, I2, M7I2, M8, M9)

  val tests = evolutions.scanLeft(Map.empty[GenericModel, Seq[TestCase]]) { case (m, evolution) =>
    m + (evolution.getModel -> evolution.tests)
  }.tail

  val transaction:Option[BranchTransaction]

  def run(args: List[String]): IO[ExitCode] = {
    val name = evolutions.head.getModel.base.name
    for {
      _ <- IO { System.out.println(s"Use: git clone http://127.0.0.1:8081/$name ${evolutions.last.getModel.name}") }
      exitCode <- new GitService(transaction.toSeq, name).runProcess("sbt test")
    } yield exitCode
  }
}
