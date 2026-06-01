package org.combinators.archive.cogen.topDown.pascal

import org.combinators.dp.TestExample
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.cogen.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.cogen.{AbstractSyntax, NameProvider, TypeRep, Understands}
import org.combinators.dp.original.Utility
import org.combinators.models.{LiteralInt, UnitExpression}
import org.combinators.archive.unenhancedModels.models.LiteralPair

/**
 * One of the earliest implementations to generate a successful top-down implementation of Pascal's Triangle WITH test cases.
 */
trait PascalObjectOrientedProvider extends Utility {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  def find_method_recursive(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      res <- ooParadigm.methodBodyCapabilities.getMember(self, name)
    } yield res
  }

  /** Define standard test name. */
  def testCaseName: paradigm.syntax.Name = {
    names.mangle("Test")
  }
  
  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val message:String = "message"
  lazy val main:String = "main"
  lazy val testName = names.mangle("TestSuite")
  lazy val compute = names.mangle("compute")

  def getter(attr:String) : String = {
    "get" + attr.capitalize
  }

  def make_compute_method_signature(): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _ <- setParameters(Seq((names.mangle("r"), intType),(names.mangle("c"), intType)))
      _ <- setReturnType(intType)

    } yield ()
  }

  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      _ <- make_compute_method_signature()
      args <- getArguments()

      func <- find_method_recursive(names.mangle("compute"))

      intType <- toTargetLanguageType(TypeRep.Int)

      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      (namer,tper,r) = args.head
      (namec,tpec,c) = args.tail.head


      /*
      //Code for HashMap (doesn't work)
      mapClass <- findClass(names.mangle("java"), names.mangle("util"), names.mangle("HashMap"))
      _ <- resolveAndAddImport(mapClass)
      mapInst <- instantiateObject(mapClass, Seq.empty, None)
      mapVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("mymap"), mapClass, Some(mapInst))
    */

      lec0 <- arithmetic.arithmeticCapabilities.le(c, zero)

      ler0 <- arithmetic.arithmeticCapabilities.le(r, zero)

      resultName <- freshName(names.mangle("result"))
      resultVar <- impParadigm.imperativeCapabilities.declareVar(resultName, intType, Some(zero))

      r_1 <- arithmetic.arithmeticCapabilities.sub(r, one)
      c_1 <- arithmetic.arithmeticCapabilities.sub(c, one)
      app1  <- apply(func, Seq(r_1,c))
      app2 <- apply(func, Seq(r_1,c_1))
      recSum <- arithmetic.arithmeticCapabilities.add(app1,app2)

      ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(lec0, for {

        assignStmt1 <- impParadigm.imperativeCapabilities.assignVar(resultVar, one)
        _ <- addBlockDefinitions(Seq(assignStmt1))
      } yield (),

        Seq((ler0,
          for {
            assignStmt2 <- impParadigm.imperativeCapabilities.assignVar(resultVar, zero)
            _ <- addBlockDefinitions(Seq(assignStmt2))
        } yield () )),

        Some(
        for {
          assignStmt3<- impParadigm.imperativeCapabilities.assignVar(resultVar, recSum)
          _ <- addBlockDefinitions(Seq(assignStmt3))
        } yield ())
      )
      _ <- addBlockDefinitions(Seq(ifStmt))

    } yield Some(resultVar)
  }
  /**
  public int pascal(int r, int c) {
        if(c==0)
            return 1;
        if(r==0)
            return 0;


        return pascal(r-1,c-1)+pascal(r-1,c);
  }
   */
  def makeSimpleDP(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle("Pascal"))
  }

  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._

    val tests = Seq(
      new TestExample("pasc11", LiteralPair(1,1), LiteralInt(1), new UnitExpression),
      new TestExample("pasc32", LiteralPair(3,2), LiteralInt(3), new UnitExpression),
      new TestExample("pasc63", LiteralPair(6,3), LiteralInt(20), new UnitExpression),
      new TestExample("pasc2013", LiteralPair(20,13), LiteralInt(77520), new UnitExpression),
    )

    for {
      assert_statements <- forEach(tests) { example =>

        val pair = example.inputType match {
          case lp:LiteralPair => (lp.val1, lp.val2)
          case _ => ???
        }

        val expected_value = example.answer match {
          case lit:LiteralInt => lit.literal
          case _ => ???
        }

        for {
          pascType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("Pascal"))
          r_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, pair._1)
          c_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, pair._2)
          sol <- ooParadigm.methodBodyCapabilities.instantiateObject(pascType, Seq.empty)
          computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, compute)

          intType <- toTargetLanguageType(TypeRep.Int)
          pascrc_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, expected_value)
          pascrc_actual <- apply(computeMethod, Seq(r_value,c_value))
          asserteq_fib <- asserts.assertionCapabilities.assertEquals(intType, pascrc_actual, pascrc_value)

        } yield asserteq_fib
      }
    } yield assert_statements
  }

  def makeTestCase(clazzName:String): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTestCase(), names.mangle(clazzName))
    } yield ()
  }

  def implement(): Generator[ProjectContext, Unit] = {

    for {
      _ <- makeSimpleDP()
      _ <- paradigm.projectCapabilities.addCompilationUnit(
        paradigm.compilationUnitCapabilities.addTestSuite(testName, makeTestCase("DP"))
      )
    } yield ()
  }
}

object PascalObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = PascalObjectOrientedProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   ffiArithmetic: Arithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   stringsIn: Strings.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   booleansIn: Booleans.WithBase[base.MethodBodyContext, base.type]
  )
  : PascalObjectOrientedProvider.WithParadigm[base.type] =
    new PascalObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      val realArithmetic: ffiRealArithmetic.type = ffiRealArithmetic
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val strings: Strings.WithBase[base.MethodBodyContext, paradigm.type] = stringsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val booleans: Booleans.WithBase[base.MethodBodyContext, paradigm.type] = booleansIn
    }
}
