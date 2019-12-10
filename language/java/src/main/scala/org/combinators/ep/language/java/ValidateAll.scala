package org.combinators.ep.language.java

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.language.java.validate.{ExtensibleVisitorValidate, InterpreterValidate, TraditionalValidate, TriviallyValidate, VisitorSideEffectValidate, VisitorValidate}

/** Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 *
 * Challenging because of the complex types we have used.
 */
object ValidateAll extends IOApp {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = CodeGenerator.PartiallyBoxed))

  val approaches = Seq(
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
    approaches.foreach( f => {
      val result = f.run(List.empty)
    })

    IO.pure(ExitCode.Success)
  }
}
