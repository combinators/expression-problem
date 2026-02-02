package org.combinators.archive.bottomUp.oneSequence.fibtest

import org.combinators.archive.bottomUp.oneSequence.OneSequencesUtility
import org.combinators.dp.Utility
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.{AbstractSyntax, NameProvider, Understands}

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait FibTestObjectOrientedProvider extends FibTestProvider with OneSequencesUtility with Utility {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

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

  /**
   * Default registration for findClass, which works with each registerTypeMapping for the different approaches.
   *
   * Sometimes the mapping is fixed for an EP approach, but sometimes it matters when a particular class is requested
   * in the evolution of the system over time.
   */
  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindClass)
  }

  /** Provides meaningful default solution to find the base data type in many object-oriented approaches.
   *
   * This enables target-language classes to be retrieved from within the code generator in the Method, Class or Constructor contexts.
   */
//  def registerTypeMapping(tpe:DataType): Generator[ProjectContext, Unit] = {
//    import ooParadigm.projectCapabilities.{addTypeLookupForClasses, addTypeLookupForConstructors}
//    import paradigm.projectCapabilities.addTypeLookupForMethods    // must be present, regardless of IntelliJ
//    val dtpe = TypeRep.DataType(tpe)
//    for {
//      _ <- addTypeLookupForMethods(dtpe, domainTypeLookup(tpe))
//      _ <- addTypeLookupForClasses(dtpe, domainTypeLookup(tpe))
//      _ <- addTypeLookupForConstructors(dtpe, domainTypeLookup(tpe))
//    } yield ()
//  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
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
      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(np1), None)
      dpVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("dp"), arrayType, Some(instantiated))

      //iterator
      iName <- freshName(names.mangle("i"))
      iVar <- impParadigm.imperativeCapabilities.declareVar(iName, intType, Some(zero))

      //Base Cases
      dpVarIndex <- array.arrayCapabilities.get(dpVar, iVar)
      bCase1 <- impParadigm.imperativeCapabilities.assignVar(dpVarIndex, zero)
      bCase2 <- impParadigm.imperativeCapabilities.assignVar(dpVarIndex, one)

      //Relation
      nm1 <-arithmetic.arithmeticCapabilities.sub(num,one)
      nm2  <-arithmetic.arithmeticCapabilities.sub(num,two)
      dpVarIndexnm1 <-array.arrayCapabilities.get(dpVar, nm1)
      dpVarIndexnm2 <-array.arrayCapabilities.get(dpVar, nm2)
      sum <- arithmetic.arithmeticCapabilities.add(dpVarIndexnm1,dpVarIndexnm2)

      relation <- impParadigm.imperativeCapabilities.assignVar(dpVarIndex, sum)

      //one sequence bottom up
      while_loop <- one_sequence_bottom_up(iVar, np1, Seq((zero, bCase1),(one, bCase2)), relation)
      _ <- addBlockDefinitions(while_loop)

      //final element
      dpVarIndexn <-array.arrayCapabilities.get(dpVar, num)

    } yield Some(dpVarIndexn)
  }
/**

  */

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

    // can only be optimized if 'forEach' is added to CoGen. Right now I think it is in EpCoGen
    val initial_vals = Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)

    for {
      solutionType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("FibTest"))
      d_0 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      d_8 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 8)
      d_4 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 4)
      d_12 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 12)
      d_2 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      d_10 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 10)
      d_6 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 6)
      d_14 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 14)
      d_1 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      d_9 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 9)
      d_5 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 5)
      d_13 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 13)
      d_3 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 3)
      d_11 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 11)
      d_7 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 7)
      d_15 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 15)

      sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solutionType, Seq.empty)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, names.mangle("compute"))
      solution_result <- apply(computeMethod, Seq(d_0, d_8, d_4, d_12, d_2, d_10, d_6, d_14, d_1, d_9, d_5, d_13, d_3, d_11, d_7, d_15))
      six  <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 6)
      asserteq2 <- asserts.assertionCapabilities.assertEquals(arrayType, solution_result, six)

    } yield Seq(asserteq2)
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
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type]
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
    }
}
