package org.combinators.dp

import scala.collection.mutable
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
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]

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
      maxCond <- arithmetic.arithmeticCapabilities.lt(maxVar, replacement)
      maxIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(maxCond, for {

        assignStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, replacement)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq.empty
      )
    } yield maxIfStmt
  }



  //This code does not work, can't access static method on class
  def full_set_max(maxVar: Expression, e1: Expression, e2: Expression): Generator[MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      mathClass <- findClass(names.mangle("Math"))
      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(mathClass, Seq.empty, None)
      method <- getMember(instantiated, names.mangle("max"))
      maxExp <- apply(method, Seq(e1, e2))
      maxStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, maxExp)
    } yield (maxStmt)
  }

  def new_full_set_max(maxVar: Expression, e1: Expression, e2: Expression): Generator[MethodBodyContext, Seq[Statement]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      set1 <- impParadigm.imperativeCapabilities.assignVar(maxVar, e1)

      intType <- toTargetLanguageType(TypeRep.Int)
      tempName <- freshName(names.mangle("temp"))
      tempVar <- impParadigm.imperativeCapabilities.declareVar(tempName, intType, None)
      tempAssign <- impParadigm.imperativeCapabilities.assignVar(tempVar, e2)

      maxCond <- arithmetic.arithmeticCapabilities.lt(maxVar, tempVar)
      maxIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(maxCond, for {
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, tempVar)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq.empty
      )
    } yield Seq(set1, tempAssign, maxIfStmt)
  }

  def plus_equals(variable: Expression, value: Expression): Generator[MethodBodyContext, Statement] = {
    for {
      addExpr <- arithmetic.arithmeticCapabilities.add(variable, value)
      assign <- impParadigm.imperativeCapabilities.assignVar(variable, addExpr)
    } yield assign
  }

  def char_at(string: Expression, index: Expression): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      method <- getMember(string, names.mangle("charAt"))
      char_at <- apply(method, Seq(index))
    } yield char_at
  }

  def declare_and_inst_variable(varName: String, varType: Type, varValueGenerator: Generator[paradigm.MethodBodyContext, Expression]): Generator[paradigm.MethodBodyContext, Expression] = {
    for {
      varValue <- varValueGenerator
      outputVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(varName), varType, Some(varValue))
    } yield outputVar
  }

  def declare_and_inst_variable(varName: String, varType: Type, varValue: Expression): Generator[paradigm.MethodBodyContext, Expression] = {
    for {
      outputVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(varName), varType, Some(varValue))
    } yield outputVar
  }

  def make_for_loop(loopCounter: Expression, condExpr: Expression, body: Seq[Statement]): Generator[paradigm.MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      while_loop <- impParadigm.imperativeCapabilities.whileLoop(condExpr, for {
        _ <- addBlockDefinitions(body)

        incrExpr <- arithmetic.arithmeticCapabilities.add(loopCounter, one)
        incrStmt <- impParadigm.imperativeCapabilities.assignVar(loopCounter, incrExpr)
        _ <- addBlockDefinitions(Seq(incrStmt))
      } yield ())

      //_ <- addBlockDefinitions(Seq(while_loop))

    } yield (while_loop)
  }

  def get_matrix_element(matrix: Expression, row: Expression, col: Expression): Generator[paradigm.MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      matrix_at_r <- array.arrayCapabilities.get(matrix, row)
      matrix_at_r_c <- array.arrayCapabilities.get(matrix_at_r, col)
    } yield matrix_at_r_c
  }

  /**
   * Helper function for nested for loops.
   *
   * @param var1          An expression representing the variable to be updated in the outer loop.
   * @param guard1        An expression representing the guard condition for the outer loop.
   * @param update1       An expression representing the update expression for the outer loop.
   * @param var2          An expression representing the variable to be updated in the inner loop.
   * @param guard2        An expression representing the guard condition for the inner loop.
   * @param update2       An expression representing the update expression for the inner loop.
   * @param inner_body    The body of the inner loop.
   * @param trailing_body Additional statements to be executed at the end of the inner loop after the execution of the inner loop.
   * @return A generator of statements for the outer loop.
   */
  def make_nested_for_loop(var1: Expression, guard1: Expression, update1: Expression,
                           var2: Expression, guard2: Expression, update2: Expression,
                           inner_body: Seq[Statement], trailing_body: Seq[Statement]): Generator[paradigm.MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      outer_loop <- impParadigm.imperativeCapabilities.whileLoop(guard1,
        for {
          inner_loop <- impParadigm.imperativeCapabilities.whileLoop(guard2,
            for {
              _ <- addBlockDefinitions(inner_body)

              updateStmt <- impParadigm.imperativeCapabilities.assignVar(var2, update2)
              _ <- addBlockDefinitions(Seq(updateStmt))
            } yield ()
          )

          _ <- addBlockDefinitions(Seq(inner_loop))

          _ <- addBlockDefinitions(trailing_body)

          updateStmt <- impParadigm.imperativeCapabilities.assignVar(var1, update1)
          _ <- addBlockDefinitions(Seq(updateStmt))
        } yield ()
      )
    } yield outer_loop
  }
}
