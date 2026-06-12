package org.combinators.archive.cogen.topDown.oneSequence.tribonacci

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.*
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.cogen.{AbstractSyntax, NameProvider, TypeRep}
import org.combinators.dp.TestExample
import org.combinators.dp.original.Utility
import org.combinators.models.{LiteralInt, UnitExpression}

/**
 * One of the earliest implementations to generate a successful top-down Tribonacci with tests.
 *
 * Still uses the no-argument constructor while passing the argument into compute() method.
 */
trait TribonacciTopDownProvider extends Utility {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val message: String = "message"
  lazy val main: String = "main"
  lazy val testName = names.mangle("TestSuite")
  lazy val dpName = names.mangle("dp")
  lazy val memoName = names.mangle("memo")

  lazy val rName = names.mangle("r")
  lazy val cName = names.mangle("c")

  var memo: Boolean = false

  lazy val resultVarName = names.mangle("result")

  def getter(attr: String): String = {
    "get" + attr.capitalize
  }

  def make_compute_method_signature(): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setParameters(Seq((names.mangle("n"), intType)))
      _ <- setReturnType(intType)

    } yield ()
  }

  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      _ <- make_compute_method_signature()
      args <- getArguments()

      self <- ooParadigm.methodBodyCapabilities.selfReference()
      func <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("compute"))

      (name, tpe, n) = args.head

      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      three <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 3)

      neq0 <- arithmetic.arithmeticCapabilities.le(n, zero)
      neq2 <- arithmetic.arithmeticCapabilities.le(n, two)

      ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(
        neq0,
        for {
          returnStmt <- impParadigm.imperativeCapabilities.returnStmt(zero)
          _ <- addBlockDefinitions(Seq(returnStmt))
        } yield (),
        Seq(
          (neq2,
            for {
              returnStmt <- impParadigm.imperativeCapabilities.returnStmt(one)
              _ <- addBlockDefinitions(Seq(returnStmt))
            } yield ()),
        ),
        Some(
          for {
            n_1 <- arithmetic.arithmeticCapabilities.sub(n, one)
            fn_1 <- apply(func, Seq(n_1))
            n_2 <- arithmetic.arithmeticCapabilities.sub(n, two)
            fn_2 <- apply(func, Seq(n_2))
            n_3 <- arithmetic.arithmeticCapabilities.sub(n, three)
            fn_3 <- apply(func, Seq(n_3))

            out <- arithmetic.arithmeticCapabilities.add(fn_1, fn_2)
            out <- arithmetic.arithmeticCapabilities.add(out, fn_3)
            returnStmt <- impParadigm.imperativeCapabilities.returnStmt(out)
            _ <- addBlockDefinitions(Seq(returnStmt))
          } yield ()
        )
      )

      _ <- addBlockDefinitions(Seq(ifStmt))
    } yield None
  }

  def makeSimpleDP(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle("Tribonacci"))
  }

  // Specific examples hard coded for Int input and Int output
  def makeTestCases(): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities.*
    import paradigm.methodBodyCapabilities.*
    import syntax._

    // 0, 1, 1, 2, 4, 7, 13, 24, ...
    //
    // 0  1  2  3  4  5  6   7
    val tests = Seq(
      new TestExample("trib0", LiteralInt(0), LiteralInt(0), new UnitExpression), // for now, leave solution as None
      new TestExample("trib1", LiteralInt(1), LiteralInt(1), new UnitExpression),
      new TestExample("trib2", LiteralInt(2), LiteralInt(1), new UnitExpression),
      new TestExample("trib7", LiteralInt(3), LiteralInt(2), new UnitExpression),
      new TestExample("trib20", LiteralInt(4), LiteralInt(4), new UnitExpression),
      new TestExample("trib40", LiteralInt(5), LiteralInt(7), new UnitExpression)
    )

    for {
      assert_statements <- forEach(tests) { example =>

        val input_value = example.inputType match {
          case lt: LiteralInt => lt.literal
          case _ => ??? // error in all other circumstances
        }

        val expected_value = example.answer match {
          case lit: LiteralInt => lit.literal
          case _ => ???
        }

        for {
          fibType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("Tribonacci"))
          n_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, input_value)
          sol <- ooParadigm.methodBodyCapabilities.instantiateObject(fibType, Seq.empty)
          computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, computeName)

          intType <- toTargetLanguageType(TypeRep.Int)
          fibn_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, expected_value)
          fib_actual <- apply(computeMethod, Seq(n_value))
          asserteq_fib <- asserts.assertionCapabilities.assertEquals(intType, fib_actual, fibn_value)

        } yield asserteq_fib
      }
    } yield assert_statements
  }

  def makeTestCase(clazzName:String): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTestCases(), names.mangle(clazzName))
    } yield ()
  }

  def implement(): Generator[ProjectContext, Unit] = {
    //
    //    var isTopDown = false
    //
    //    option match {
    //      case td: TopDown =>
    //        memo = td.memo
    //        isTopDown = true
    //      case _: BottomUp =>
    //        isTopDown = false
    //    }
    //
    //    for {
    //      _ <- if (isTopDown) {
    //        make_top_down(model)
    //      } else {
    //        make_bottom_up(model)
    //      }
    //    } yield ()

    for {
      _ <- makeSimpleDP()
            _ <- paradigm.projectCapabilities.addCompilationUnit(
              paradigm.compilationUnitCapabilities.addTestSuite(testName, makeTestCase("DP"))
            )
    } yield None
  }
}

object TribonacciTopDownProvider {
  type WithParadigm[P <: AnyParadigm] = TribonacciTopDownProvider {val paradigm: P}
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   ffiArithmetic: Arithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   stringsIn: Strings.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   parametricPolymorphism: ParametricPolymorphism.WithBase[base.type],
   booleansIn: Booleans.WithBase[base.MethodBodyContext, base.type]
  )
  (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): TribonacciTopDownProvider.WithParadigm[base.type] =
    new TribonacciTopDownProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      val realArithmetic: ffiRealArithmetic.type = ffiRealArithmetic
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: oo.type = oo
      override val polymorphics: parametricPolymorphism.type = parametricPolymorphism
      override val genericsParadigm: generics.type = generics
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val strings: Strings.WithBase[base.MethodBodyContext, paradigm.type] = stringsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val booleans: Booleans.WithBase[base.MethodBodyContext, paradigm.type] = booleansIn
    }
}