package org.combinators.archive.cogen.bottomUp.oneSequence.maxsubarray

import org.combinators.dp.TestExample
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.cogen.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.cogen.{AbstractSyntax, NameProvider, TypeRep}
import org.combinators.dp.original.Utility
import org.combinators.models.{LiteralArray, LiteralInt}

/**
 * One of the earliest implementations to generate a successful bottom-up implementation of MaxSubArray
 *
 * Successfully used "new_full_set_max" helper method which generated code to compute maximum inside while loop.
 */
trait MaxSubarrayObjectOrientedProvider extends Utility {
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
      _ <- setParameters(Seq((names.mangle("nums"), arrayType)))
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

      (namen,tpen,nums) = args.head

      // manually create variables 'c', 'm', 'i' to use
      currentName <- freshName(names.mangle("c"))
      numsZ <- array.arrayCapabilities.get(nums, Seq(zero))
      currentVar <- impParadigm.imperativeCapabilities.declareVar(currentName, intType, Some(numsZ))

      maxName <- freshName(names.mangle("m"))
      maxVar <- impParadigm.imperativeCapabilities.declareVar(maxName, intType, Some(currentVar))

      iName <- freshName(names.mangle("i"))
      iVar <- impParadigm.imperativeCapabilities.declareVar(iName, intType, Some(one))

      numsLength <- ooParadigm.methodBodyCapabilities.getMember(nums, names.mangle("length"))
      whileCond <- arithmetic.arithmeticCapabilities.lt(iVar,numsLength)

      init_stmt <- impParadigm.imperativeCapabilities.whileLoop(whileCond, for {

        // the BODY
        curIfStmt <- set_max(currentVar,zero)

        //Current assignment
        numsI <- array.arrayCapabilities.get(nums, Seq(iVar))
        currentAssign <- plus_equals(currentVar,numsI)

        //Set Max
        maxIfStmt <- new_full_set_max(maxVar,maxVar, currentVar)

        // last line to be added to the while loop
        //incrExpr <- arithmetic.arithmeticCapabilities.add(iVar, one)
        incrStmt <- plus_equals(iVar, one)
        _ <- addBlockDefinitions(Seq(curIfStmt, currentAssign)++maxIfStmt++Seq( incrStmt))
      } yield ()
      )
      _ <- addBlockDefinitions(Seq(init_stmt))

    } yield Some(maxVar)
  }
/** ACTUAL JAVA IMPLEMENTATION AS CODED MANUALLY. USE AS REFERENCE
  class MaxSubarray {
    public int solution(int[] nums) {

      int c=nums[0];
      int m=c;

      for(int i=1;i<nums.length;i++){
        if(c<0){
          c=0;
        }
        c+=nums[i];
        m=Math.max(m,c);
      }

      return m;
    }
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

    addClassToProject(makeClass, names.mangle("MaxSubarray"))
  }


  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._
    import AnyParadigm.syntax._

    // https://en.wikipedia.org/wiki/Maximum_subarray_problem
    val wiki_test = new TestExample("wiki",
        LiteralArray(Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)),
      LiteralInt(6),
      LiteralArray(Array(4, -1, 2, 1))
    )

    // https://www.geeksforgeeks.org/dsa/largest-sum-contiguous-subarray/
    val geeks_for_geeks_test = new TestExample("geeks_for_geeks",
      LiteralArray(Array(2, 3, -8, 7, -1, 2, 3)),
      LiteralInt(11),
      LiteralArray(Array(7, -1, 2, 3))
    )

    // https://leetcode.com/problems/maximum-subarray/description/
    val leetcode_test = new TestExample("leetcode",
      LiteralArray(Array(5, 4, -1, 7, 8)),
      LiteralInt(23),
      LiteralArray(Array(5, 4, -1, 7, 8))
    )

    for {
      solutionType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("MaxSubarray"))
      sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solutionType, Seq.empty)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, names.mangle("compute"))

      assert_statements <- forEach(Seq(wiki_test, geeks_for_geeks_test, leetcode_test)) { example =>

        val array_vals = example.inputType match {
          case la:LiteralArray => la.literal
          case _ => ???
        }

        val expected_value = example.answer match {
          case lit:LiteralInt => lit.literal
          case _ => ???
        }

        for {
          expr <- create_int_array(array_vals)
          variable <- impParadigm.imperativeCapabilities.declareVar(names.mangle(example.name), arrayType, Some(expr))
          invoke <- apply(computeMethod, Seq(variable))
          solution <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, expected_value)
          assert_stmt <- asserts.assertionCapabilities.assertEquals(arrayType, invoke, solution)

          // still need test case for validating full_solution when calling 'retrieve()'

        } yield assert_stmt
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

object MaxSubarrayObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = MaxSubarrayObjectOrientedProvider { val paradigm: P }
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
   stringsIn : Strings.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   booleansIn: Booleans.WithBase[base.MethodBodyContext, base.type]
  )
  : MaxSubarrayObjectOrientedProvider.WithParadigm[base.type] =
    new MaxSubarrayObjectOrientedProvider {
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
