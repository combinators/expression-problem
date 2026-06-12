package org.combinators.archive.cogen.bottomUp.oneSequence.fibtest

import org.combinators.archive.cogen.bottomUp.oneSequence.OneSequencesUtility
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.cogen.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.cogen.{AbstractSyntax, NameProvider, TypeRep}
import org.combinators.dp.original.Utility

/**
 * One of the earliest implementations to generate a successful bottom-up implementation of Fibonacci with test cases.
 *
 * Note that it attempted to use "one_sequence_bottom_up" but this method never fully worked with multiple sequences of base cases
 */
trait FibTestObjectOrientedProvider extends OneSequencesUtility with Utility {
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

  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val message:String = "message"
  lazy val main:String = "main"
  lazy val testName = names.mangle("TestSuite")

  def getter(attr:String) : String = {
    "get" + attr.capitalize
  }

  def make_compute_method_signature(): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _ <- setParameters(Seq((names.mangle("num"), intType)))
      _ <- setReturnType(intType)

    } yield ()
  }

  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- make_compute_method_signature()
      args <- getArguments()

      func <- find_method_recursive(names.mangle("compute"))

      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))

      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      (namen,tpen,num) = args.head

      np1 <- arithmetic.arithmeticCapabilities.add(num,one)

      //Instantiate
      instantiated <- array.arrayCapabilities.create(intType /* arrayType*/, Seq(np1), None)
      dpVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("dp"), arrayType, Some(instantiated))

      //iterator
      iName <- freshName(names.mangle("i"))
      iVar <- impParadigm.imperativeCapabilities.declareVar(iName, intType, Some(zero))

      //Base Cases
      dpVarIndex <- array.arrayCapabilities.get(dpVar, Seq(iVar))
      bCase1 <- impParadigm.imperativeCapabilities.assignVar(dpVarIndex, iVar)

      //Relation
      nm1 <-arithmetic.arithmeticCapabilities.sub(iVar,one)
      nm2  <-arithmetic.arithmeticCapabilities.sub(iVar,two)
      dpVarIndexnm1 <-array.arrayCapabilities.get(dpVar, Seq(nm1))
      dpVarIndexnm2 <-array.arrayCapabilities.get(dpVar, Seq(nm2))
      sum <- arithmetic.arithmeticCapabilities.add(dpVarIndexnm1,dpVarIndexnm2)

      relation <- impParadigm.imperativeCapabilities.assignVar(dpVarIndex, sum)

      //one sequence bottom up
      while_loop <- one_sequence_bottom_up(iVar, np1, Seq((one, bCase1)), relation)
      _ <- addBlockDefinitions(while_loop)

      //final element
      dpVarIndexn <-array.arrayCapabilities.get(dpVar, Seq(num))

    } yield Some(dpVarIndexn)
  }


  def makeSimpleDP(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle("FibTest"))
  }


  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      solutionType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("FibTest"))
      d_0 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      d_1 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      d_2 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      d_9 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 9)
      d_11 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 11)
      d_89 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 89)
      d_34 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 34)

      sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solutionType, Seq.empty)
      intType <- toTargetLanguageType(TypeRep.Int)
      computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, names.mangle("compute"))

      solution_result <- apply(computeMethod, Seq(d_0))
      asserteq0 <- asserts.assertionCapabilities.assertEquals(intType, solution_result, d_0)

      solution_result <- apply(computeMethod, Seq(d_2))
      asserteq1 <- asserts.assertionCapabilities.assertEquals(intType, solution_result, d_1)

      solution_result <- apply(computeMethod, Seq(d_9))
      asserteq2 <- asserts.assertionCapabilities.assertEquals(intType, solution_result, d_34)

      solution_result <- apply(computeMethod, Seq(d_11))
      asserteq3 <- asserts.assertionCapabilities.assertEquals(intType, solution_result, d_89)

    } yield Seq(asserteq0,asserteq1,asserteq2,asserteq3)
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

object FibTestObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = FibTestObjectOrientedProvider { val paradigm: P }
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
  : FibTestObjectOrientedProvider.WithParadigm[base.type] =
    new FibTestObjectOrientedProvider {
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
