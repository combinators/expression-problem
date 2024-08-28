package org.combinators.fibonacci

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Assertions, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Functional, control}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider}

/**
 * Take advantage of observation that:
 *
 *   Lucas(n) = Fib(n-1) + F(n+1) for n > 1
 *   Fib(x+y) = [(Fib(x)*Lucas(y) + Fib(y)*Locas(x)] / 2
 *
 * With this you can declare
 *
 *   n1 = floor(n/2)
 *   n2 = n - n1
 *   Fib(n) = [(Fib(n1)*Lucas(n2) + Fib(n2)*Locas(n1)] / 2
 *
 * And the resulting speed up is quite impressive
 */
trait FibonacciWithLucas {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val functionalParadigm: Functional.WithBase[paradigm.type]
  val functionalControlParadigm: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  lazy val fibName:paradigm.syntax.Name  = names.mangle("fib")
  lazy val lucasName:paradigm.syntax.Name  = names.mangle("lucas")
  lazy val testFibName:paradigm.syntax.Name  = names.mangle("fibTest")
  lazy val nName:paradigm.syntax.Name  = names.mangle("n")

  def make_lucas(): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import functionalControlParadigm.functionalCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)

      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)
      args <- getArguments()
      func <- functionalParadigm.methodBodyCapabilities.findMethod(Seq(fibName))
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      le0 <- ffiArithmetic.arithmeticCapabilities.le(args.head._3, zero)
      le1 <- ffiArithmetic.arithmeticCapabilities.le(args.head._3, one)
      
      subexpr <- ffiArithmetic.arithmeticCapabilities.sub(args.head._3, one)
      addexpr <- ffiArithmetic.arithmeticCapabilities.add(args.head._3, one)
      subcall <- apply(func, Seq(subexpr))
      addcall <- apply(func, Seq(addexpr))
      addExpr <- ffiArithmetic.arithmeticCapabilities.add(subcall, addcall)

      res <- ifThenElse(le0, Command.lift(two), Seq.empty,
        ifThenElse(le1, Command.lift(one), Seq.empty, Command.lift(addExpr))
      )

    } yield res
  }

  def make_fibonacci(): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import functionalControlParadigm.functionalCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)
      args <- getArguments()

      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      three <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 3)

      /*
       *   n1 = floor(n/2)
       *   n2 = n - n1
       *   Fib(n) = [(Fib(n1)*Lucas(n2) + Fib(n2)*Lucas(n1)] / 2
       */
      n1 <- ffiArithmetic.arithmeticCapabilities.div(args.head._3, two)
      n2 <- ffiArithmetic.arithmeticCapabilities.sub(args.head._3, n1)

      func_fib <- functionalParadigm.methodBodyCapabilities.findMethod(Seq(fibName))
      func_lucas <- functionalParadigm.methodBodyCapabilities.findMethod(Seq(lucasName))

      n1_fib <- apply(func_fib, Seq(n1))
      n2_lucas <- apply(func_lucas, Seq(n2))

      n1_lucas <- apply(func_lucas, Seq(n1))
      n2_fib <- apply(func_fib, Seq(n2))

      mult1Expr <- ffiArithmetic.arithmeticCapabilities.mult(n1_fib,n2_lucas)
      mult2Expr <- ffiArithmetic.arithmeticCapabilities.mult(n1_lucas,n2_fib)
      addExpr <- ffiArithmetic.arithmeticCapabilities.add(mult1Expr, mult2Expr)
      resExpr <- ffiArithmetic.arithmeticCapabilities.div(addExpr, two)

      le0 <- ffiArithmetic.arithmeticCapabilities.le(args.head._3, zero)
      le1 <- ffiArithmetic.arithmeticCapabilities.le(args.head._3, one)
      le2 <- ffiArithmetic.arithmeticCapabilities.le(args.head._3, two)
      le3 <- ffiArithmetic.arithmeticCapabilities.le(args.head._3, three)
      res <- ifThenElse(le0, Command.lift(zero), Seq.empty,
        ifThenElse(le1, Command.lift(one), Seq.empty,
          ifThenElse(le2, Command.lift(one), Seq.empty,
            ifThenElse(le3, Command.lift(two), Seq.empty, Command.lift(resExpr)))))

    } yield res
  }

  def make_unit(): Generator[paradigm.CompilationUnitContext, Unit] = {
    for {
      _ <- functionalParadigm.compilationUnitCapabilities.addMethod(fibName, make_fibonacci())
      _ <- functionalParadigm.compilationUnitCapabilities.addMethod(lucasName, make_lucas())
    } yield ()
  }

  def make_fibonacci_test(): Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] = {
    import ffiEquality.equalityCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      func <- functionalParadigm.methodBodyCapabilities.findMethod(Seq(fibName))
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      check0 <- apply(func, Seq(zero))
      asserteq0 <- ffiAssertions.assertionCapabilities.assertEquals(intType, check0, zero)

      check1 <- apply(func, Seq(one))
      asserteq1 <- ffiAssertions.assertionCapabilities.assertEquals(intType, check1, one)

      seven <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 7)
      thirteen <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 13)
      check7 <- apply(func, Seq(seven))
      asserteq7 <- ffiAssertions.assertionCapabilities.assertEquals(intType, check7, thirteen)

      lucasfunc <- functionalParadigm.methodBodyCapabilities.findMethod(Seq(lucasName))
      twentynine <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 29)
      check7 <- apply(lucasfunc, Seq(seven))
      asserteq8 <- ffiAssertions.assertionCapabilities.assertEquals(intType, check7, twentynine)

    } yield Seq(asserteq0, asserteq1, asserteq7, asserteq8)
  }

  def make_test() : Generator[paradigm.TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(make_fibonacci_test(), testFibName)
    } yield ()
  }

  def make_project(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- paradigm.projectCapabilities.addCompilationUnit(make_unit(), fibName)
      _ <- paradigm.projectCapabilities.addCompilationUnit(paradigm.compilationUnitCapabilities.addTestSuite(testFibName, make_test()), testFibName)
    } yield ()
  }
}

object FibonacciWithLucasProvider {
  type WithParadigm[P <: AnyParadigm] = FibonacciWithLucas { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   func:  Functional.WithBase[base.type],
   c1: control.Functional.WithBase[base.MethodBodyContext, base.type],
   c2: Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   c3: Assertions.WithBase[base.MethodBodyContext, base.type],
   c4: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : FibonacciWithLucasProvider.WithParadigm[base.type] =
    new FibonacciWithLucas {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val functionalParadigm: Functional.WithBase[paradigm.type] = func
      override val functionalControlParadigm: control.Functional.WithBase[paradigm.MethodBodyContext, paradigm.type] = c1
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = c2
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = c3
      override val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = c4
    }
}
