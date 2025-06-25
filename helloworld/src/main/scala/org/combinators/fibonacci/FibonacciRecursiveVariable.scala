package org.combinators.fibonacci

import org.combinators.cogen.abstractions.TypeRep
import org.combinators.cogen.paradigm.{AnyParadigm, Functional}
import org.combinators.cogen.paradigm.control.Functional
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Assertions, Equality}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider}

trait FibonacciRecursiveVariable {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val functionalParadigm: Functional.WithBase[paradigm.type]
  val functionalControlParadigm: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  lazy val fibName:paradigm.syntax.Name  = names.mangle("fib")
  lazy val innerFibName: paradigm.syntax.Name = names.mangle("inner_loop")
  lazy val fibPackage:paradigm.syntax.Name  = names.mangle("fibonacci")
  lazy val testFibName:paradigm.syntax.Name  = names.mangle("fibTest")
  lazy val nName:paradigm.syntax.Name  = names.mangle("n")


  def make_fibonacci_lambda(func: paradigm.syntax.Expression): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import functionalControlParadigm.functionalCapabilities._
    import paradigm.methodBodyCapabilities._
    import functionalControlParadigm.lambdaCapabilities._

    def inner_lambda(params: Map[paradigm.syntax.Name, paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] =
      for {
        one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
        two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
        (_,n) = params.toList.head
        le1 <- ffiArithmetic.arithmeticCapabilities.le(n, one)
        n_1 <- ffiArithmetic.arithmeticCapabilities.sub(n, one)
        n_2 <- ffiArithmetic.arithmeticCapabilities.sub(n, two)
        fn_1 <- apply(func, Seq(n_1))
        fn_2 <- apply(func, Seq(n_2))
        addExpr <- ffiArithmetic.arithmeticCapabilities.add(fn_1, fn_2)
        res <- ifThenElse(le1, Command.lift(n), Seq.empty, Command.lift(addExpr))
      } yield res

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      lam <- lambda(Seq((nName, intType)), inner_lambda)
    } yield lam
  }

  def make_fibonacci(): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    import functionalControlParadigm.functionalCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      lamType <- toTargetLanguageType(TypeRep.Arrow(TypeRep.Int, TypeRep.Int))

      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)
      args <- getArguments()

      res <- declareRecursiveVariable(innerFibName, lamType, make_fibonacci_lambda)(innerFib => {
        for {
          args <- getArguments()
          (name,tpe,n) = args.head
          r <- apply(innerFib, Seq(n))
        } yield r
      })


    } yield res
  }


  def make_unit(): Generator[paradigm.CompilationUnitContext, Unit] = {
    for {
      _ <- functionalParadigm.compilationUnitCapabilities.addMethod(fibName, make_fibonacci())
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

    } yield Seq(asserteq0, asserteq1, asserteq7)
  }

  def make_test() : Generator[paradigm.TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(make_fibonacci_test(), testFibName)
    } yield ()
  }

  def make_project(): Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- paradigm.projectCapabilities.addCompilationUnit(make_unit(), fibPackage)
      _ <- paradigm.projectCapabilities.addCompilationUnit(paradigm.compilationUnitCapabilities.addTestSuite(testFibName, make_test()), testFibName)
    } yield ()
  }
}

object FibonacciRecursiveVariableProvider {
  type WithParadigm[P <: AnyParadigm] = FibonacciRecursiveVariable { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   func:  Functional.WithBase[base.type],
   c1: Functional.WithBase[base.MethodBodyContext, base.type],
   c2: Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   c3: Assertions.WithBase[base.MethodBodyContext, base.type],
   c4: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : FibonacciRecursiveVariableProvider.WithParadigm[base.type] =
    new FibonacciRecursiveVariable {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val functionalParadigm: Functional.WithBase[paradigm.type] = func
      override val functionalControlParadigm: Functional.WithBase[paradigm.MethodBodyContext, paradigm.type] = c1
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = c2
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = c3
      override val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = c4
    }
}
