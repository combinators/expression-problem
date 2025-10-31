package org.combinators.dp

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait DPObjectOrientedProvider extends DPProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  import paradigm._
  import syntax._
  import ooParadigm._

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
  def registerTypeMapping(tpe:DataType): Generator[ProjectContext, Unit] = {
    import paradigm.projectCapabilities.addTypeLookupForMethods
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod          // must be present, regardless of IntelliJ
    import ooParadigm.projectCapabilities.addTypeLookupForClasses
    import ooParadigm.projectCapabilities.addTypeLookupForConstructors
    import ooParadigm.classCapabilities.canFindClassInClass                // must be present, regardless of IntelliJ
    import ooParadigm.constructorCapabilities.canFindClassInConstructor    // must be present, regardless of IntelliJ
    val dtpe = TypeRep.DataType(tpe)
    for {
      _ <- addTypeLookupForMethods(dtpe, domainTypeLookup(tpe))
      _ <- addTypeLookupForClasses(dtpe, domainTypeLookup(tpe))
      _ <- addTypeLookupForConstructors(dtpe, domainTypeLookup(tpe))
    } yield ()
  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
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
      _ <- setParameters(Seq((names.mangle("nums"), arrayType)))
      _ <- setReturnType(intType)

    } yield ()
  }

  def addToCurrentContext(stmts: Generator[paradigm.MethodBodyContext, Seq[Statement]]) : Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

      for {
      ss <- stmts
      _ <- addBlockDefinitions(ss)
    } yield ()
  }

  // ,
  //               elseBranch:Option[Generator[paradigm.MethodBodyContext, Seq[Statement]]]
  def iftemplate(guard:Expression,
               ifBranch:Generator[paradigm.MethodBodyContext, Seq[Statement]])
    : Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- impParadigm.imperativeCapabilities.ifThenElse(guard, for {
        stmts <- ifBranch
        _ <- addBlockDefinitions(stmts)
      } yield (), Seq.empty)
    } yield ()
  }

  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      _ <- make_compute_method_signature()
      args <- getArguments()

      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      array2dType <- toTargetLanguageType(TypeRep.Array(TypeRep.Array(TypeRep.Int)))
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      // isn't there an array capability to get length?
      numsLength <- ooParadigm.methodBodyCapabilities.getMember(args.head._3, names.mangle("length"))
      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(numsLength), None)

      dpVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("dp"), arrayType, Some(instantiated))  // won't work
      iName <- freshName(names.mangle("i"))
      iVar <- impParadigm.imperativeCapabilities.declareVar(iName, intType, Some(zero))
      jName <- freshName(names.mangle("j"))
      jVar <- impParadigm.imperativeCapabilities.declareVar(jName, intType, Some(zero))

      condExpr <- arithmetic.arithmeticCapabilities.lt(iVar, numsLength)
      init_stmt <- impParadigm.imperativeCapabilities.whileLoop(condExpr, for {

        // the BODY
        dpVarIndex <- array.arrayCapabilities.get(dpVar, iVar)
        stmt1 <- impParadigm.imperativeCapabilities.assignVar(dpVarIndex, one)

        // last line to be added to the while loop
        incrExpr <- arithmetic.arithmeticCapabilities.add(iVar, one)
        incrStmt <- impParadigm.imperativeCapabilities.assignVar(iVar, incrExpr)
        _ <- addBlockDefinitions(Seq(stmt1, incrStmt))
      } yield ()
      )
      _ <- addBlockDefinitions(Seq(init_stmt))

      maxName <- freshName(names.mangle("max"))
      maxVar <- impParadigm.imperativeCapabilities.declareVar(maxName, intType, Some(zero))
      setI <- impParadigm.imperativeCapabilities.assignVar(iVar, zero)

      outer_stmt <- impParadigm.imperativeCapabilities.whileLoop(condExpr, for {

        // the BODY
        condjiExpr <- arithmetic.arithmeticCapabilities.lt(jVar, iVar)
        setJ <- impParadigm.imperativeCapabilities.assignVar(jVar, zero)

        inner_stmt <- impParadigm.imperativeCapabilities.whileLoop(condjiExpr, for {

          // the BODY
          numIindex <- array.arrayCapabilities.get(args.head._3, iVar)
          numJIndex <- array.arrayCapabilities.get(args.head._3, jVar)

          ifExpr <- arithmetic.arithmeticCapabilities.lt(numJIndex, numIindex)

          maxIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(ifExpr, for {

            dpVarIndex <- array.arrayCapabilities.get(dpVar, iVar)
            dpVarJIndex <- array.arrayCapabilities.get(dpVar, jVar)
            dpVarJIndexPlusOne <- arithmetic.arithmeticCapabilities.add(dpVarJIndex, one)

            ifInnerExpr <- arithmetic.arithmeticCapabilities.lt(dpVarIndex, dpVarJIndexPlusOne)

            dpInnerIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(ifInnerExpr, for {

              dpVarIndex <- array.arrayCapabilities.get(dpVar, iVar)
              dpVarJIndex <- array.arrayCapabilities.get(dpVar, jVar)
              dpVarJIndexPlusOne <- arithmetic.arithmeticCapabilities.add(dpVarJIndex, one)

              assignStmt <- impParadigm.imperativeCapabilities.assignVar(dpVarIndex, dpVarJIndexPlusOne)
              _ <- addBlockDefinitions(Seq(assignStmt))
            } yield (),
              Seq.empty
            )

            _ <- addBlockDefinitions(Seq(dpInnerIfStmt ))
          } yield (),
            Seq.empty
          )

          // last line to be added to the while loop
          incrExpr <- arithmetic.arithmeticCapabilities.add(jVar, one)
          incrjStmt <- impParadigm.imperativeCapabilities.assignVar(jVar, incrExpr)
          _ <- addBlockDefinitions(Seq(maxIfStmt, incrjStmt))
        } yield ()
        )

        dpVarIndex2 <- array.arrayCapabilities.get(dpVar, iVar)
        ifMaxExpr <- arithmetic.arithmeticCapabilities.lt(maxVar, dpVarIndex2)
        maxCheckIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(ifMaxExpr, for {

          assignStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, dpVarIndex2)
          _ <- addBlockDefinitions(Seq(assignStmt))
        } yield (),
          Seq.empty
        )

        // last line to be added to the while loop
        incrExpr <- arithmetic.arithmeticCapabilities.add(iVar, one)
        incriStmt <- impParadigm.imperativeCapabilities.assignVar(iVar, incrExpr)
        _ <- addBlockDefinitions(Seq(setJ, inner_stmt, maxCheckIfStmt, incriStmt))
      } yield ()
      )

      _ <- addBlockDefinitions(Seq(setI, outer_stmt))

    } yield Some(maxVar)
  }
  /**
  public class Solution {
    public int compute (int[] nums) {
        int[] dp = new int[nums.length];
        for (int i = 0; i < dp.length; i++) {
          dp[i] = 1;
        }
        int max=0;
        for (int i=0; i < dp.length; i++) {
            for (int j=0; j<i; j++){
                if (nums[j] < nums[i]) {
                    dp[i] = Math.max(dp[i], dp[j]+1);
                }
            }
            max = Math.max(max,dp[i]);
        }
        return max;
    }
}*/
  def makeSimpleDP(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle("Solution"))
  }


  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import eqls.equalityCapabilities._

    // can only be optimized if 'forEach' is added to CoGen. Right now I think it is in EpCoGen
    val initial_vals = Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)

    for {
      solutionType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("Solution"))
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
    import ooParadigm.projectCapabilities._
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

object DPObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = DPObjectOrientedProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   ffiArithmetic: Arithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : DPObjectOrientedProvider.WithParadigm[base.type] =
    new DPObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
    }
}
