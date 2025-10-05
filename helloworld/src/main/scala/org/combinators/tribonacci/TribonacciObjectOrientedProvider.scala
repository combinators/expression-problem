package org.combinators.tribonacci

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.{AbstractSyntax, NameProvider, Understands}

trait TribonacciObjectOrientedProvider extends TribonacciProvider {
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

  lazy val message: String = "message"
  lazy val main: String = "main"
  lazy val testName = names.mangle("TestSuite")

  def getter(attr: String): String = {
    "get" + attr.capitalize
  }

  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindClass)
  }

  def registerTypeMapping(tpe: DataType): Generator[ProjectContext, Unit] = {
    import paradigm.projectCapabilities.addTypeLookupForMethods
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import ooParadigm.projectCapabilities.addTypeLookupForClasses
    import ooParadigm.projectCapabilities.addTypeLookupForConstructors
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor

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
      _ <- setParameters(Seq((names.mangle("n"), intType)))
      _ <- setReturnType(intType)

    } yield ()
  }

  /**
   * public class Solution {
   *    public int compute(int n) {
   *        if(n == 0) {
   *            return 0;
   *        } else if(n <= 2) {
   *            return 1;
   *        } else {
   *            int[] dp = new int[n + 1];
   *            dp[0] = 0;
   *            dp[1] = 1;
   *            dp[2] = 1;
   *
   *            int i = 3;
   *            while(i <= n) {
   *                dp[i] = dp[i - 1] + dp[i - 2] + dp[i - 3];
   *                i = i + 1;
   *            }
   *        }
   *
   *        return dp[n];
   *    }
   * }
   */
  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      _ <- make_compute_method_signature()
      args <- getArguments()

      (name, tpe, n) = args.head

      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      three <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 3)

      resultName <- freshName(names.mangle("result"))
      resultVar <- impParadigm.imperativeCapabilities.declareVar(resultName, intType, Some(zero))

//      n <= 0 todo: replace le with eq
      len0 <- arithmetic.arithmeticCapabilities.le(n, zero)

//      n <= 2
      len2 <- arithmetic.arithmeticCapabilities.le(n, two)

      ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(len0, for {
        assignStmt1 <- impParadigm.imperativeCapabilities.assignVar(resultVar, zero)
        _ <- addBlockDefinitions(Seq(assignStmt1))
      } yield (),
        Seq((len2,
          for {
            assignStmt2 <- impParadigm.imperativeCapabilities.assignVar(resultVar, one)
            _ <- addBlockDefinitions(Seq(assignStmt2))
          } yield ()
        )),

        Some(
          for {
//            int[] dp = new int[n + 1]; todo: increment n by 1
            dpLength <- ooParadigm.methodBodyCapabilities.getMember(n, names.mangle("length"))
            instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(n), None)
            dpVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("dp"), arrayType, Some(instantiated))

//            dp[0] = 0;
            dpVar0 <- array.arrayCapabilities.get(dpVar, zero)
            baseCase0 <- impParadigm.imperativeCapabilities.assignVar(dpVar0, zero)
            _ <- addBlockDefinitions(Seq(baseCase0))

//            dp[1] = 1;
            dpVar1 <- array.arrayCapabilities.get(dpVar, one)
            baseCase1 <- impParadigm.imperativeCapabilities.assignVar(dpVar1, one)
            _ <- addBlockDefinitions(Seq(baseCase1))


//            dp[2] = 1;
            dpVar2 <- array.arrayCapabilities.get(dpVar, two)
            baseCase2 <- impParadigm.imperativeCapabilities.assignVar(dpVar2, one)
            _ <- addBlockDefinitions(Seq(baseCase2))

            iName <- freshName(names.mangle("i"))
            iVar <- impParadigm.imperativeCapabilities.declareVar(iName, intType, Some(three))

            condExpr <- arithmetic.arithmeticCapabilities.le(iVar, n)

            buildUp <- impParadigm.imperativeCapabilities.whileLoop(condExpr, for {
//              dp[i - 1]
              dpi_1 <- arithmetic.arithmeticCapabilities.sub(iVar, one)
              dpi_1val <- array.arrayCapabilities.get(dpVar, dpi_1)

//              dp[i - 2]
              dpi_2 <- arithmetic.arithmeticCapabilities.sub(iVar, two)
              dpi_2val <- array.arrayCapabilities.get(dpVar, dpi_2)

//              dp[i - 3]
              dpi_3 <- arithmetic.arithmeticCapabilities.sub(iVar, three)
              dpi_3val <- array.arrayCapabilities.get(dpVar, dpi_3)

//              dp[n] = dp[n - 1] + dp[n - 2] + dp[n - 3];
              dpi <- array.arrayCapabilities.get(dpVar, iVar)
              dpival <- arithmetic.arithmeticCapabilities.add(dpi_1val, dpi_2val)
              dpival <- arithmetic.arithmeticCapabilities.add(dpival, dpi_3val)
              dpiAssign <- impParadigm.imperativeCapabilities.assignVar(dpi, dpival)
              _ <- addBlockDefinitions(Seq(dpiAssign))

//              i = i + 1
              incrExpr <- arithmetic.arithmeticCapabilities.add(iVar, one)
              incrStmt <- impParadigm.imperativeCapabilities.assignVar(iVar, incrExpr)
              _ <- addBlockDefinitions(Seq(incrStmt))

            } yield ()
            )

            _ <- addBlockDefinitions(Seq(buildUp))

            dpn <- array.arrayCapabilities.get(dpVar, n)
            assignStmt3 <- impParadigm.imperativeCapabilities.assignVar(resultVar, dpn)
            _ <- addBlockDefinitions(Seq(assignStmt3))
          } yield ()
        )
      )
      _ <- addBlockDefinitions(Seq(ifStmt))

    } yield Some(resultVar)
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

//  todo: make test cases
//  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
//    import paradigm.methodBodyCapabilities._
//    import eqls.equalityCapabilities._
//
//    for {
//      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
//    } yield ()
//  }

  def implement(): Generator[ProjectContext, Unit] = {

    for {
      _ <- makeSimpleDP()
//      _ <- paradigm.projectCapabilities.addCompilationUnit(
//        paradigm.compilationUnitCapabilities.addTestSuite(testName, makeTestCase("DP"))
//      )
    } yield ()
  }
}

object TribonacciObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = TribonacciObjectOrientedProvider { val paradigm: P }
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
  : TribonacciObjectOrientedProvider.WithParadigm[base.type] =
    new TribonacciObjectOrientedProvider {
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