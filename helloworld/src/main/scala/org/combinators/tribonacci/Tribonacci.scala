package org.combinators.tribonacci

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Assertions, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Functional, control}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider}

trait Tribonacci {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val functionalParadigm: Functional.WithBase[paradigm.type]
  val functionalControlParadigm: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  lazy val tribName: paradigm.syntax.Name = names.mangle("trib")
  lazy val tribPackage: paradigm.syntax.Name = names.mangle("tribonacci")
  lazy val testTribName: paradigm.syntax.Name = names.mangle("tribTest")
  lazy val nName: paradigm.syntax.Name = names.mangle("n")

  def make_tribonacci(): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import functionalControlParadigm._
    import paradigm.methodBodyCapabilities._

    for {

    } yield res
  }
}