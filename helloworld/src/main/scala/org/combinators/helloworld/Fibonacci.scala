package org.combinators.helloworld

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider}
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Assertions, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Functional, control}

trait Fibonacci {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val functionalParadigm: Functional.WithBase[paradigm.type]
  val functionalControlParadigm: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  lazy val fibName = names.mangle("fib")
  lazy val testFibName = names.mangle("fibTest")
  lazy val nName = names.mangle("n")


  def make_fibonacci(): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import functionalControlParadigm.functionalCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)

      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)
      args <- getArguments()
      func <- functionalParadigm.methodBodyCapabilities.findMethod(Seq(fibName))
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      le1 <- ffiArithmetic.arithmeticCapabilities.le(args.head._3, one)
      subexpr1 <- ffiArithmetic.arithmeticCapabilities.sub(args.head._3, one)
      subexpr2 <- ffiArithmetic.arithmeticCapabilities.sub(args.head._3, two)
      subcall1 <- apply(func, Seq(subexpr1))
      subcall2 <- apply(func, Seq(subexpr2))
      addExpr <- ffiArithmetic.arithmeticCapabilities.add(subcall1, subcall2)
      res <- ifThenElse(le1, Command.lift(one), Seq.empty, Command.lift(addExpr))
    } yield res
  }

  def make_unit(): Generator[paradigm.CompilationUnitContext, Unit] = {
    for {
      _ <- functionalParadigm.compilationUnitCapabilities.addMethod(fibName, make_fibonacci())
    } yield ()
  }

  def make_fibonacci_test(): Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ffiEquality.equalityCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      func <- functionalParadigm.methodBodyCapabilities.findMethod(Seq(fibName))
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      check0 <- apply(func, Seq(zero))
      asserteq0 <- ffiAssertions.assertionCapabilities.assertEquals(intType, check0, one)

      check1 <- apply(func, Seq(one))
      asserteq1 <- ffiAssertions.assertionCapabilities.assertEquals(intType, check1, one)

      seven <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 7)
      twentyone <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 21)
      check7 <- apply(func, Seq(seven))
      asserteq7 <- ffiAssertions.assertionCapabilities.assertEquals(intType, check7, twentyone)

    } yield Seq(asserteq0, asserteq1, asserteq7)
  }

  def make_test() : Generator[paradigm.TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(make_fibonacci_test(), testFibName)
    } yield ()
  }

  def make_project(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- paradigm.projectContextCapabilities.addCompilationUnit(make_unit(), fibName)
      _ <- paradigm.projectContextCapabilities.addCompilationUnit(paradigm.compilationUnitCapabilities.addTestSuite(testFibName, make_test()), testFibName)
    } yield ()
  }
}

object FibonacciProvider {
  type WithParadigm[P <: AnyParadigm] = Fibonacci { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   func:  Functional.WithBase[base.type],
   c1: control.Functional.WithBase[base.MethodBodyContext, base.type],
   c2:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   c3:  Assertions.WithBase[base.MethodBodyContext, base.type],
   c4 : Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : FibonacciProvider.WithParadigm[base.type] =
    new Fibonacci {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val functionalParadigm: Functional.WithBase[paradigm.type] = func
      override val functionalControlParadigm: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type] = c1
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = c2
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = c3
      override val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = c4
    }
}
