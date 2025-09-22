package org.combinators.tribonacci

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.generator.paradigm.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Assertions, Equality}

trait TribonacciIndependent {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  lazy val tribName: paradigm.syntax.Name = names.mangle("trib")
  lazy val testTribName: paradigm.syntax.Name = names.mangle("tribTest")
  lazy val nName: paradigm.syntax.Name = names.mangle("n")

  type IfBlockType

  def if_then_else(cond: paradigm.syntax.Expression,
                   ifBlock: Generator[paradigm.MethodBodyContext, IfBlockType],
                   ifElseBlocks: Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
                   elseBlock: Generator[paradigm.MethodBodyContext, IfBlockType]):
    Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def find_method_recursive(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]

  def find_method(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]

  def add_method(name: paradigm.syntax.Name,
                 spec: Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]):
    Generator[paradigm.CompilationUnitContext, Unit]

  def return_in_if(toReturn: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]):
    Generator[paradigm.MethodBodyContext, IfBlockType]

  def make_tribonacci(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {

    }
  }
}