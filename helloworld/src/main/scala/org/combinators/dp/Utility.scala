package org.combinators.dp

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}

trait Utility {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]

  import paradigm._
  import syntax._
  import ooParadigm._

  def make_for_loop(loopCounter: Expression, condExpr: Expression, body: Seq[Statement]): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      while_loop <- impParadigm.imperativeCapabilities.whileLoop(condExpr, for
      {
        _ <- addBlockDefinitions(body)

        incrExpr <- arithmetic.arithmeticCapabilities.add(loopCounter, one)
        incrStmt <- impParadigm.imperativeCapabilities.assignVar(loopCounter, incrExpr)
        _ <- addBlockDefinitions(Seq(incrStmt))
      } yield () )

      _ <- addBlockDefinitions(Seq(while_loop))

    } yield ()
  }
}
