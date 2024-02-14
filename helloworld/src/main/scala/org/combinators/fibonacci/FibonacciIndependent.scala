package org.combinators.fibonacci

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Assertions, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Functional, ObjectOriented, control}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider}

trait FibonacciIndependent {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val ffiAssertions : Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  lazy val fibName:paradigm.syntax.Name  = names.mangle("fib")
  lazy val testFibName:paradigm.syntax.Name = names.mangle("fibTest")
  lazy val nName:paradigm.syntax.Name  = names.mangle("n")

  type IfBlockType

  def if_then_else(cond: paradigm.syntax.Expression,
                   ifBlock: Generator[paradigm.MethodBodyContext, IfBlockType],
                   ifElseBlocks: Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])],
                   elseBlock: Generator[paradigm.MethodBodyContext, IfBlockType]):
      Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]

  def find_method_recursive(name:paradigm.syntax.Name) : Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]

  // used for tests
  def find_method(name:paradigm.syntax.Name) : Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]

  // add method (in OO this would require construction of a class first...)
  def add_method(name: paradigm.syntax.Name,
                 spec: Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]
                 ): Generator[paradigm.CompilationUnitContext, Unit]

  def return_in_if(toReturn:Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, IfBlockType]

  def make_fibonacci(): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)

      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((nName, intType)))
      _ <- paradigm.methodBodyCapabilities.setReturnType(intType)
      args <- getArguments()

      // recursive
      func <- find_method_recursive(fibName)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      (name,tpe,n) = args.head
      le1 <- ffiArithmetic.arithmeticCapabilities.le(n, one)
      n_1 <- ffiArithmetic.arithmeticCapabilities.sub(n, one)
      n_2 <- ffiArithmetic.arithmeticCapabilities.sub(n, two)
      fn_1 <- apply(func, Seq(n_1))
      fn_2 <- apply(func, Seq(n_2))
      addExpr <- ffiArithmetic.arithmeticCapabilities.add(fn_1, fn_2)

      // for n <= 1 just return n. This ensures F0=0 and F1=1
      res <- if_then_else(le1, return_in_if(Command.lift(n)), Seq.empty, return_in_if(Command.lift(addExpr)))
    } yield res
  }

  def make_unit(): Generator[paradigm.CompilationUnitContext, Unit] = {
    for {
      _ <- add_method(fibName, make_fibonacci())
    } yield ()
  }

  def make_fibonacci_test(): Generator[paradigm.MethodBodyContext, Seq[paradigm.syntax.Expression]] = {
    import ffiEquality.equalityCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      func <- find_method(fibName)
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
      _ <- paradigm.projectCapabilities.addCompilationUnit(make_unit(), fibName)
      _ <- paradigm.projectCapabilities.addCompilationUnit(paradigm.compilationUnitCapabilities.addTestSuite(testFibName, make_test()), testFibName)
    } yield ()
  }
}

object FibonacciIndependentProvider {
  type WithParadigm[P <: AnyParadigm] = FibonacciIndependent { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def functional[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   func:  Functional.WithBase[base.type],
   c1: control.Functional.WithBase[base.MethodBodyContext, base.type],
   c2:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   c3:  Assertions.WithBase[base.MethodBodyContext, base.type],
   c4: Equality.WithBase[base.MethodBodyContext, base.type],
  )
  : FibonacciIndependentProvider.WithParadigm[base.type] =
    new FibonacciIndependent {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider

      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = c2
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = c3
      override val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = c4

      override type IfBlockType = paradigm.syntax.Expression

      override def if_then_else(cond: paradigm.syntax.Expression, ifBlock: Generator[paradigm.MethodBodyContext, IfBlockType], ifElseBlocks: Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])], elseBlock: Generator[paradigm.MethodBodyContext, IfBlockType]): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        for {
          res <- c1.functionalCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, elseBlock)
        } yield Some(res)
      }

      override def find_method_recursive(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = find_method(name)
      override def find_method(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        func.methodBodyCapabilities.findMethod(Seq(name))
      }

      override def add_method(name: paradigm.syntax.Name, spec: Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]): Generator[paradigm.CompilationUnitContext, Unit] = {
        func.compilationUnitCapabilities.addMethod(name, spec.map(x => x.get))
      }

      override def return_in_if(toReturn: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, IfBlockType] = {
        toReturn
      }
    }

  def imperative[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   obj:  ObjectOriented.WithBase[base.type],
   c1: control.Imperative.WithBase[base.MethodBodyContext, base.type],
   c2:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   c3:  Assertions.WithBase[base.MethodBodyContext, base.type],
   c4: Equality.WithBase[base.MethodBodyContext, base.type],
  )
  : FibonacciIndependentProvider.WithParadigm[base.type] =
    new FibonacciIndependent {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider

      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = c2
      override val ffiAssertions: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type] = c3
      override val ffiEquality : Equality.WithBase[paradigm.MethodBodyContext, paradigm.type] = c4

      override type IfBlockType = Unit
      lazy val fibClass:paradigm.syntax.Name  = names.mangle("Fib")

      override def if_then_else(cond: paradigm.syntax.Expression, ifBlock: Generator[paradigm.MethodBodyContext, IfBlockType], ifElseBlocks: Seq[(paradigm.syntax.Expression, Generator[paradigm.MethodBodyContext, IfBlockType])], elseBlock: Generator[paradigm.MethodBodyContext, IfBlockType]): Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]] = {
        for {
          resultStmt <- c1.imperativeCapabilities.ifThenElse(cond, ifBlock, ifElseBlocks, Some(elseBlock))
          _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(resultStmt))
        } yield None
      }

      override def find_method_recursive(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        for {
          self <- obj.methodBodyCapabilities.selfReference()
          res <- obj.methodBodyCapabilities.getMember(self, name)
        } yield res
      }

      override def find_method(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
        for {
          typ <- obj.methodBodyCapabilities.findClass(fibClass)
          fib <- obj.methodBodyCapabilities.instantiateObject(typ, Seq.empty)
          res <- obj.methodBodyCapabilities.getMember(fib, name)
        } yield res
      }
      override def add_method(name: paradigm.syntax.Name, spec: Generator[paradigm.MethodBodyContext, Option[paradigm.syntax.Expression]]): Generator[paradigm.CompilationUnitContext, Unit] = {
        obj.compilationUnitCapabilities.addClass(fibClass, obj.classCapabilities.addMethod(name, spec))
      }

      override def return_in_if(toReturn: Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression]): Generator[paradigm.MethodBodyContext, IfBlockType] = {
        for {
          resultExp <- toReturn
          resultStmt <- c1.imperativeCapabilities.returnStmt(resultExp)
          _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(resultStmt))
        } yield None
      }
    }
}
