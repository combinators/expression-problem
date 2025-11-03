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

  /**
   * Helper for max operations, represents 'm=max(m,r)'
   * Returns the generated max statement
   */
  def set_max(maxVar: Expression, replacement: Expression): Generator[MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    for {
      maxCond <- arithmetic.arithmeticCapabilities.lt(maxVar,replacement)
      maxIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(maxCond, for {

        assignStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, replacement)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq.empty
      )
    } yield maxIfStmt
  }

  def full_set_max(maxVar: Expression, e1: Expression, e2: Expression): Generator[MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for{
      mathClass <- findClass(names.mangle("Math"))
      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(mathClass, Seq.empty, None)
      method <- getMember(instantiated,names.mangle("max"))
      maxExp <- apply(method, Seq(e1,e2))
      maxStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar,maxExp)
    }yield(maxStmt)
  }

  def new_full_set_max(maxVar: Expression, e1: Expression, e2: Expression): Generator[MethodBodyContext, Seq[Statement]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      set1 <- impParadigm.imperativeCapabilities.assignVar(maxVar, e1)

      intType <- toTargetLanguageType(TypeRep.Int)
      tempName <- freshName(names.mangle("temp"))
      tempVar <- impParadigm.imperativeCapabilities.declareVar(tempName, intType, None)
      tempAssign <-impParadigm.imperativeCapabilities.assignVar(tempVar, e2)

      maxCond <- arithmetic.arithmeticCapabilities.lt(maxVar,tempVar)
      maxIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(maxCond, for {
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, tempVar)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq.empty
      )
    } yield Seq(set1,tempAssign,maxIfStmt)
  }

  def plus_equals(variable: Expression, value: Expression): Generator[MethodBodyContext, Statement]={
    for {
      addExpr <- arithmetic.arithmeticCapabilities.add(variable,value)
      assign <- impParadigm.imperativeCapabilities.assignVar(variable, addExpr)
    } yield assign
  }

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
