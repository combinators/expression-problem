package org.combinators.archive.topDown.oneSequence.DeleteAndEarn

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator._
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.Booleans.WithBase
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}

/** Provider for House Robber DP solution in OO style. */
trait DeleteAndEarnObjectOrientedProvider extends DeleteAndEarnProvider {
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

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  def find_method_recursive(name: paradigm.syntax.Name): Generator[paradigm.MethodBodyContext, paradigm.syntax.Expression] = {
    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      res <- ooParadigm.methodBodyCapabilities.getMember(self, name)
    } yield res
  }

  def makeHelperSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for{
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _<- setParameters(
        Seq((names.mangle("arr"), arrayType))
      )
      _<- setReturnType(arrayType)
    } yield()
  }

  def makeDeleteAndEarnSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for{
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _<- setParameters(
        Seq((names.mangle("arr"), arrayType),
          (names.mangle("index"), intType))
      )
      _<- setReturnType(intType)
    } yield()
  }

  def makeSolutionSignature(): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for{
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _<- setParameters(
        Seq((names.mangle("arr"), arrayType))
      )
      _<- setReturnType(intType)
    } yield()
  }

  /*
  public void bubbleSort(int[] array) {
        boolean swapped = true;
        int j = 0;
        int tmp;
        while (swapped) {
            swapped = false;
            j++;
            for (int i = 0; i < array.length - j; i++) {
                if (array[i] > array[i + 1]) {
                    tmp = array[i];
                    array[i] = array[i + 1];
                    array[i + 1] = tmp;
                    swapped = true;
                }
            }
        }
    }
   */

  def Bubble_sort(): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for{
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      boo_true <- paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, true)
      boo_false <- paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, false)
      _<- makeHelperSignature()
      args <- getArguments()
      (namen,tpen,arr) = args.head
      intType <- toTargetLanguageType(TypeRep.Int)
      booleanType <- toTargetLanguageType(TypeRep.Boolean)
      size <- array.arrayCapabilities.length(arr)

      swapped <- impParadigm.imperativeCapabilities.declareVar(names.mangle("swapped"), booleanType, Some(boo_true))
      jVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("j"), intType, Some(zero))
      tmp <- impParadigm.imperativeCapabilities.declareVar(names.mangle("tmp"), intType)
      whileStmt <- impParadigm.imperativeCapabilities.whileLoop(swapped, for{
        swap_fal <- impParadigm.imperativeCapabilities.assignVar(swapped, boo_false)
        _<-addBlockDefinitions(Seq(swap_fal))

        j_one <- arithmetic.arithmeticCapabilities.add(jVar, one)
        jVarExp <- impParadigm.imperativeCapabilities.assignVar(jVar, j_one)
        _<-addBlockDefinitions(Seq(jVarExp))

        index <- impParadigm.imperativeCapabilities.declareVar(names.mangle("i"), intType, Some(zero))
        arrSize_j <- arithmetic.arithmeticCapabilities.sub(size, jVar)
        for_cond <- arithmetic.arithmeticCapabilities.lt(index, arrSize_j)
        index_plusOne <- arithmetic.arithmeticCapabilities.add(index, one)
        inner_forLoop <- impParadigm.imperativeCapabilities.whileLoop(for_cond, for{
          arr_index <- array.arrayCapabilities.get(arr, index)
          arr_index_one <- array.arrayCapabilities.get(arr, index_plusOne)
          inner_ifCond <- arithmetic.arithmeticCapabilities.lt(arr_index_one, arr_index)

          innerIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(inner_ifCond, for{
            assignTMP <- impParadigm.imperativeCapabilities.assignVar(tmp, arr_index)
            assignArrIndex <- impParadigm.imperativeCapabilities.assignVar(arr_index, arr_index_one)
            assignArrOneIndex <- impParadigm.imperativeCapabilities.assignVar(arr_index_one, arr_index)
            swap_true <- impParadigm.imperativeCapabilities.assignVar(swapped, boo_true)
            _<-addBlockDefinitions(Seq(assignTMP, assignArrIndex, assignArrOneIndex, swap_true))

          } yield(),
            Seq.empty)
          _<-addBlockDefinitions(Seq(innerIfStmt))

        } yield())
        _<- addBlockDefinitions(Seq(inner_forLoop))
      } yield())
      _<-addBlockDefinitions(Seq(whileStmt))


    } yield(Some(arr))
  }

  /*
  private int dfs(int[] nums, int i) {
        if (i >= nums.length) return 0;

        int cur = nums[i], pick = 0;
        while (i < nums.length && nums[i] == cur) {
            pick += nums[i];
            i++;
        }

        int res = dfs(nums, i);
        while (i < nums.length && nums[i] == cur + 1) {
            i++;
        }

        res = Math.max(res, pick + dfs(nums, i));
        return res;
    }
   */
  def helper(): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for{
      _ <- makeDeleteAndEarnSignature()
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      args <- getArguments()
      intType <- toTargetLanguageType(TypeRep.Int)
      (names1, tpes1, arr) = args.head
      (names2, tpes2, index) = args.tail.head
      arrSize <- array.arrayCapabilities.length(arr)
      func <- find_method_recursive(names.mangle("helper"))

      index_1 <- arithmetic.arithmeticCapabilities.add(index, one)

      ans <- impParadigm.imperativeCapabilities.declareVar(names.mangle("result"), intType)

      ifCond1 <- arithmetic.arithmeticCapabilities.lt(arrSize, index)
      ifCond2 <- eqls.equalityCapabilities.areEqual(intType, arrSize, index)
      ifCond <- booleans.booleanCapabilities.or(Seq(ifCond1, ifCond2))
      ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(ifCond, for{
        ansExpr <- impParadigm.imperativeCapabilities.assignVar(ans, zero)
        _<- addBlockDefinitions(Seq(ansExpr))
      } yield(),
        Seq.empty,
        Some(
          for{
            arr_index <- array.arrayCapabilities.get(arr, index)
            cursor <- impParadigm.imperativeCapabilities.declareVar(names.mangle("cur"), intType, Some(arr_index))
            pick <- impParadigm.imperativeCapabilities.declareVar(names.mangle("pick"), intType, Some(zero))

            whilecond1 <- arithmetic.arithmeticCapabilities.lt(index, arrSize)
            whilecond2 <- eqls.equalityCapabilities.areEqual(intType, cursor, arr_index)
            whileCond <- booleans.booleanCapabilities.and(Seq(whilecond1, whilecond2))
            innerwhileStmt <- impParadigm.imperativeCapabilities.whileLoop(whileCond, for{
              pickValue <- arithmetic.arithmeticCapabilities.add(pick, arr_index)
              pickExpr <- impParadigm.imperativeCapabilities.assignVar(pick, pickValue)
              indexExpr <- impParadigm.imperativeCapabilities.assignVar(index, index_1)
              _<-addBlockDefinitions(Seq(pickExpr, indexExpr))
            } yield())
            _<-addBlockDefinitions(Seq(innerwhileStmt))

            resValue <- apply(func, Seq(arr, index))
            ansExpr <- impParadigm.imperativeCapabilities.assignVar(ans, resValue)
            _<-addBlockDefinitions(Seq(ansExpr))

            cursor_1 <- arithmetic.arithmeticCapabilities.add(cursor, one)
            whilecond3 <- eqls.equalityCapabilities.areEqual(intType, arr_index, cursor_1)
            whileCond <- booleans.booleanCapabilities.and(Seq(whilecond1, whilecond3))
            innerwhileStmt <- impParadigm.imperativeCapabilities.whileLoop(whileCond, for{
              indexExpr <- impParadigm.imperativeCapabilities.assignVar(index, index_1)
              _<-addBlockDefinitions(Seq(indexExpr))
            } yield())
            _<-addBlockDefinitions(Seq(innerwhileStmt))

            pick_plusAns <- arithmetic.arithmeticCapabilities.add(pick, resValue)
            ifcond <- arithmetic.arithmeticCapabilities.lt(ans, pick_plusAns)
            innerIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(ifcond, for{
              ansExpr <- impParadigm.imperativeCapabilities.assignVar(ans, pick_plusAns)
              _<-addBlockDefinitions(Seq(ansExpr))
            } yield(),
              Seq.empty,
              )

            _<-addBlockDefinitions(Seq(innerIfStmt))

          } yield()
        ))

      _<-addBlockDefinitions(Seq(ifStmt))
    } yield(Some(ans))
  }

  /*
  public int solution(int[] nums) {
        bubbleSort(nums);
        return dfs(nums, 0);
    }
   */

  def solution(): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for{
      _ <- makeSolutionSignature()
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      args <- getArguments()
      intType <- toTargetLanguageType(TypeRep.Int)
      (names1, tpes1, arr) = args.head
      sort_func <- find_method_recursive(names.mangle("Bubble_sort"))
      recur_func <- find_method_recursive(names.mangle("helper"))
      sorted_arr <- apply(sort_func, Seq(arr))
      ansValue <- apply(recur_func, Seq(sorted_arr, zero))

    } yield(Some(ansValue))
  }





  /** Create the HouseRobber class with rob() method. */
  def makeClass(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("Bubble_sort"), Bubble_sort())
        _ <- addMethod(names.mangle("helper"), helper())
        _<- addMethod(names.mangle("solution"), solution())

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
      _ <- makeClass("DeleteAndEarn")
      _ <- makeMainClass("Main")
    } yield ()
  }
}

object DeleteAndEarnObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = DeleteAndEarnObjectOrientedProvider {val paradigm: P}
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
  : DeleteAndEarnObjectOrientedProvider.WithParadigm[base.type] =
    new DeleteAndEarnObjectOrientedProvider {
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

