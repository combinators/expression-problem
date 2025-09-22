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
      intType <- toTargetLanguageType(TypeRep.Int)

      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)
      args <- getArguments()
      func <- functionalParadigm.methodBodyCapabilities.findMethod(Seq(tribName))
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      (name,tpe,n) = args.head
      le1 <- ffiArithmetic.arithmeticCapabilities.le(n, one)
      le2 <- ffiArithmetic.arithmeticCapabilities.le(n, two)
      n_1 <- ffiArithmetic.arithmeticCapabilities.sub(n, one)
      n_2 <- ffiArithmetic.arithmeticCapabilities.sub(n, two)
      fn_1 <- apply(func, Seq(n_1))
      fn_2 <- apply(func, Seq(n_2))
    } yield ()
  }

  def make_unit(): Generator[paradigm.CompilationUnitContext, Unit] = {
    for {

    } yield ()
  }

  def make_tribonacci_test(): Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] = {
    import ffiEquality.equalityCapabilities._
    import paradigm.methodBodyCapabilities._

    for {

    } yield ()
  }

  def make_test() : Generator[paradigm.TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(, testTribName)
    } yield ()
  }

  def make_project(): Generator[paradigm.ProjectContext, Unit] = {
    for {

    } yield ()
  }
}

object TribonacciProvider {
  type WithParadigm[P <: AnyParadigm] = Tribonacci { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   func: Functional.WithBase[base.type],
   c1: control.Functional.WithBase[base.MethodBodyContext, base.type],
   c2: Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   c3: Assertions.WithBase[base.MethodBodyContext, base.type],
   c4: Equality.WithBase[base.MethodBodyContext, base.type]
  ): TribonacciProvider.WithParadigm[base.type] =
    new Tribonacci {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val functionalParadigm: Functional.WithBase[paradigm.type] = func
      override val functionalControlParadigm: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type] = c1
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = c2
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = c3
      override val ffiEquality: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = c4
    }
}