package org.combinators.ep.language.java     /*DI:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import org.combinators.ep.language.java.validate.{ExtensibleVisitorValidate, InterpreterValidate, TraditionalValidate, TriviallyValidate, VisitorSideEffectValidate, VisitorValidate}

/** Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 *
 * Challenging because of the complex types we have used. Can't get this to work....
 */
@deprecated
object ValidateAll extends IOApp {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  val approaches = List(
    ExtensibleVisitorValidate,
    InterpreterValidate,
    TraditionalValidate,
    TriviallyValidate,
    VisitorValidate,
    VisitorSideEffectValidate
  )

  def run(args: List[String]): IO[ExitCode] = {
    var numSuccess = 0

    // Can't get this to work. Seems like each of the other execs stops program
    for {
      _ <- approaches.map { f => f.run(List.empty) }.sequence_
    } yield ExitCode.Success
  }
}
