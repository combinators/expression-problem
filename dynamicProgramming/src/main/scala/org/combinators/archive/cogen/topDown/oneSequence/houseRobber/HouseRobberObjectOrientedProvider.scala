package org.combinators.archive.cogen.topDown.oneSequence.houseRobber

import org.combinators.cogen.Command.Generator
import org.combinators.cogen._
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.Booleans.WithBase
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality}
import org.combinators.cogen.paradigm.{AnyParadigm, ObjectOriented}

/**
 * One of the earliest implementations to solve HouseRobber but NO TEST CASES.
 */
trait HouseRobberObjectOrientedProvider {
  val paradigm: AnyParadigm
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]

  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: WithBase[paradigm.MethodBodyContext, paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /*
  int rob(int[] hval, int n){
    int result;
    if(n <= 0){
    result = 0;
    } else if(n == 1){
    result = hval[0];
    } else {
    int pick = hval[n-1] + rob(hval, n-2);
    int notpick = rob(hval, n-1);
    if(pick < notpick){
    result = notpick;
    } else {
    result = pick;
    }
    }
    return result
  }
   */

  def find_method_recursive(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      res <- ooParadigm.methodBodyCapabilities.getMember(self, name)
    } yield res
  }

  /** Method signature: int rob(int[] nums) */
  def makeRobSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _ <- setParameters(Seq((names.mangle("nums"), arrayType)))
      _ <- setReturnType(intType)
    } yield ()
  }

  // public int some(int[] hval, int b) {
  //
  def makeSomeSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _ <- setParameters(Seq(
        (names.mangle("hval"), arrayType),
        (names.mangle("b"), intType)))
      _ <- setReturnType(intType)
    } yield ()
  }

  def makeSolutionSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for{
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setParameters(Seq((names.mangle("hval"), arrayType)))
      _ <- setReturnType(intType)
    } yield()
  }

  def make_solution(): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for{
      _ <- makeSolutionSignature()
      args <- getArguments()
      intType <- toTargetLanguageType(TypeRep.Int)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      (namen,tpen,nums) = args.head
      func <- find_method_recursive(names.mangle("rob"))
      size <- array.arrayCapabilities.length(nums, Seq.empty)

      resultExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("result"), intType, Some(zero))
      lengthExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("n"), intType, Some(size))
     // assignStmt <- impParadigm.imperativeCapabilities.assignVar(lengthExpr, size)
     // _ <- addBlockDefinitions(Seq(assignStmt))

      resultValue <- apply(func, Seq(nums, lengthExpr))
      assignStmt <- impParadigm.imperativeCapabilities.assignVar(resultExpr, resultValue)
      _ <- addBlockDefinitions(Seq(assignStmt))



    } yield(Some(resultExpr))
  }

  def someMethodImplementation(): Generator[MethodBodyContext, Option[Expression]] = {
    for {
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      _ <- makeSomeSignature()
    } yield Some(zero)
  }

    /** Method implementation: top-down DP with memoization (pseudocode). */
  def robMethodImplementation(): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      _ <- makeRobSignature()
      // Pseudocode for body:
      // int n = nums.length;
      // int[] memo = new int[n];
      // Arrays.fill(memo, -1);
      // return helper(0, nums, memo);
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      intType <- toTargetLanguageType(TypeRep.Int)

      iVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("i"), intType, Some(one))  // won't work

      // Integer i = 1;
      // if (i <= 0) {
      //    i = 0;
      //
      guard1 <- arithmetic.arithmeticCapabilities.lt(iVar, zero)
      guard2 <- eqls.equalityCapabilities.areEqual(intType, iVar, zero)
      combined_guard <- booleans.booleanCapabilities.or(Seq(guard1, guard2))
      innerIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(combined_guard, for {
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(iVar, zero)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq.empty
      )
      _ <- addBlockDefinitions(Seq(innerIfStmt))

      // In practice: declare vars, loop, call helper (another method).
    } yield Some(iVar)
  }

  def make_maxLoot_compute_signature(): Generator[paradigm.MethodBodyContext, Unit] ={
    import paradigm.methodBodyCapabilities._

    for{
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _ <- setParameters(Seq(
        (names.mangle("hval"), arrayType),
        (names.mangle("n"), intType)))
      _ <- setReturnType(intType)
    } yield ()
  }

  /**

   // Base case: no more houses
   int return_value;
   if (i < nums.length) {

   if (dp[i] != -1) {
   return_value = dp[i];
   } else {

     int robCurrent = nums[i] + helper(nums, i + 2, dp);

   int skipCurrent = helper(nums, i + 1, dp);

   dp[i] = Math.max(robCurrent, skipCurrent);

   return_value = dp[i];
   }
   } else {
   return_value = 0;
   }

   return return_value;

   * @return
   */

  def make_maxloot_compute(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      _ <- make_maxLoot_compute_signature()
      arg <- getArguments()

      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      intType <- toTargetLanguageType(TypeRep.Int)

      (names1, tpes1, hval) = arg.head
      (names2, tpes2, n) = arg.tail.head

      func <- find_method_recursive(names.mangle("rob"))

      guard_nLt0 <- arithmetic.arithmeticCapabilities.lt(n, zero)
      guard_nEQ0 <- eqls.equalityCapabilities.areEqual(intType, n, zero)
      combined_guard <- booleans.booleanCapabilities.or(Seq(guard_nLt0, guard_nEQ0))
      guard2 <- eqls.equalityCapabilities.areEqual(intType, n, one)
      size <- array.arrayCapabilities.length(hval, Seq.empty)

      n_1 <- arithmetic.arithmeticCapabilities.sub(n, one)
      n_2 <- arithmetic.arithmeticCapabilities.sub(n, two)
      houseHoldValue <- array.arrayCapabilities.get(hval, Seq(n_1))

      notpickExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("notpick"), intType, Some(zero))
      notpick2Expr <- apply(func, Seq(hval, n_1))
      notPickStmt <- impParadigm.imperativeCapabilities.assignVar(notpickExpr, notpick2Expr)
      _ <- addBlockDefinitions(Seq(notPickStmt))

      pickExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("pick"), intType, Some(zero))
      pick2Expr <- apply(func, Seq(hval, n_2))
      pickResult <- arithmetic.arithmeticCapabilities.add(houseHoldValue, pick2Expr)
      pickStmt <- impParadigm.imperativeCapabilities.assignVar(pickExpr, pickResult)
      _ <- addBlockDefinitions(Seq(pickStmt))

      hval_0_ <- array.arrayCapabilities.get(hval, Seq(zero))

      // set hval[0] = 1
      indexedLocation <- array.arrayCapabilities.get(hval, Seq(zero))
      setIL <- impParadigm.imperativeCapabilities.assignVar(indexedLocation, one)

      returnresult <- impParadigm.imperativeCapabilities.declareVar(names.mangle("result"), intType)
      returnStmt <- impParadigm.imperativeCapabilities.assignVar(returnresult, zero)

      _<- addBlockDefinitions(Seq(returnStmt))

      guard_Inner <- arithmetic.arithmeticCapabilities.lt(pickExpr, notpickExpr)

      innerIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(combined_guard, for {
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(returnresult, zero)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq((guard2, for{
          assignStmt <- impParadigm.imperativeCapabilities.assignVar(returnresult, hval_0_)
          _ <- addBlockDefinitions(Seq(assignStmt))
        } yield(),

        )
        ),
        Some(
          for{
            inner2IfStmt <- impParadigm.imperativeCapabilities.ifThenElse(guard_Inner, for{
              assignStmt <- impParadigm.imperativeCapabilities.assignVar(returnresult, notpickExpr)
              _ <- addBlockDefinitions(Seq(assignStmt))
            } yield(),
              Seq.empty,
              Some(
                for{
                  assignStmt2 <- impParadigm.imperativeCapabilities.assignVar(returnresult, pickExpr)
                  _ <- addBlockDefinitions(Seq(assignStmt2))
                } yield()
              ))

            _ <- addBlockDefinitions(Seq(inner2IfStmt))

          } yield()
        )
      )
      _ <- addBlockDefinitions(Seq(innerIfStmt))

      innerIfStmt2 <- impParadigm.imperativeCapabilities.ifThenElse(guard2, for {
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(returnresult, hval_0_)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq.empty
      )

    } yield Some(returnresult)
  }

  /**
   *

   if(pick < notpick){
   result = notpick;
   } else if(notpick < pick){
   result = pick;
   }
   */
  /** Create the HouseRobber class with rob() method. */
  def makeClass(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("rob"), make_maxloot_compute())
        _ <- addMethod(names.mangle("solution"), make_solution())
      } yield None
    }
    addClassToProject(makeClass, names.mangle(clazzName))
  }

  /** Main class to test rob() method */
  def makeMainClass(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("main"), staticMainImplementation())
      } yield ()
    }
    addClassToProject(makeClass, names.mangle(clazzName))
  }

  /** Example main implementation */
  def staticMainImplementation(): Generator[MethodBodyContext, Option[Expression]] = {
    import impParadigm.imperativeCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      _ <- setStatic()
      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitType)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      nums <- declareVar(names.mangle("nums"), arrayType, None) // in real impl: initialize
      // Example call: HouseRobber.rob(nums)
      // Example print result
    } yield None
  }

  /** Entry point */
  def implement(): Generator[ProjectContext, Unit] = {
    for {
      _ <- makeClass("HouseRobber")
      _ <- makeMainClass("Main")
    } yield ()
  }
}

object HouseRobberObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = HouseRobberObjectOrientedProvider {val paradigm: P}
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
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   boolsIn: Booleans.WithBase[base.MethodBodyContext, base.type]
  )
  : HouseRobberObjectOrientedProvider.WithParadigm[base.type] =
    new HouseRobberObjectOrientedProvider {
      override val paradigm: base.type = base
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val booleans: Booleans.WithBase[base.MethodBodyContext, paradigm.type] = boolsIn
    }
}

