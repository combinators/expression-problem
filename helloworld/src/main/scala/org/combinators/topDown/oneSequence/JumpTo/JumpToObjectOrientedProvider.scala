package org.combinators.JumpTo

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.generator._
import org.combinators.ep.generator.paradigm.ffi.Booleans.WithBase

/** Provider for House Robber DP solution in OO style. */
trait JumpToObjectOrientedProvider extends JumpToProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]

  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

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

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
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
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      _<- setParameters(
        Seq((names.mangle("arr"), arrayType))
      )
      _<- setReturnType(intType)
    } yield()
  }

  def makeMinJumpRecSignature(): Generator[MethodBodyContext, Unit] = {
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

  def makeminJumpsSignature(): Generator[MethodBodyContext, Unit] = {
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
  int retrieveMax(int[] arr) {
        int max = 0;
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] > max) {
                max = arr[i];
            }
        }
        return max+1;
    }
   */

  def retireve_max(): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for{
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      _<- makeHelperSignature()
      args <- getArguments()
      (namen,tpen,arr) = args.head
      intType <- toTargetLanguageType(TypeRep.Int)
      size <- array.arrayCapabilities.length(arr)

      ansExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("ans"), intType, Some(zero))
      iVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("i"), intType, Some(zero))
      arrSizeExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("size"), intType, Some(size))
      whileCond <- arithmetic.arithmeticCapabilities.lt(iVar,arrSizeExpr)

      for_stmt <- impParadigm.imperativeCapabilities.whileLoop(whileCond, for{
        arr_index <- array.arrayCapabilities.get(arr, iVar)
        if_cond <- arithmetic.arithmeticCapabilities.lt(ansExpr, arr_index)
        innerIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(if_cond, for{
          assignStmt <- impParadigm.imperativeCapabilities.assignVar(ansExpr, arr_index)
          _ <- addBlockDefinitions(Seq(assignStmt))
        } yield(),
          Seq.empty)
        _ <- addBlockDefinitions(Seq(innerIfStmt))

        iVar_one <- arithmetic.arithmeticCapabilities.add(iVar, one)
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(iVar, iVar_one)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield())

      _ <- addBlockDefinitions(Seq(for_stmt))

      ans_1 <- arithmetic.arithmeticCapabilities.add(ansExpr, one)
      assignStmt <- impParadigm.imperativeCapabilities.assignVar(ansExpr, ans_1)
      _<- addBlockDefinitions(Seq(assignStmt))
    } yield(Some(ansExpr))
  }

  /*
  int minJumpsRecur(int i, int[] arr) {
        int ans = 0;
            if (i < arr.length - 1){
                ans = retrieveMax(arr);
                for (int j = i + 1; j <= i + arr[i]; j++) {
                    int val = minJumpsRecur(j, arr);
                    if (val != retrieveMax(arr) && 1+val < ans) {
                        ans = val+1;
                    }
                }
            }
            return ans;
        }
   */
  def minJumpRec(): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for{
      _ <- makeMinJumpRecSignature()
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      args <- getArguments()
      intType <- toTargetLanguageType(TypeRep.Int)
      (names1, tpes1, arr) = args.head
      (names2, tpes2, index) = args.tail.head
      retrieve_func <- find_method_recursive(names.mangle("retrieveMax"))
      recur_func <- find_method_recursive(names.mangle("minJumpsRecur"))
      maxValue <- apply(retrieve_func, Seq(arr))

      ansExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("ans"), intType, Some(zero))
      arr_size <- array.arrayCapabilities.length(arr)
      arrLength_1 <- arithmetic.arithmeticCapabilities.sub(arr_size, one)

      if_cond <- arithmetic.arithmeticCapabilities.lt(index, arrLength_1)
      if_Stmt <- impParadigm.imperativeCapabilities.ifThenElse(if_cond, for{
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(ansExpr, maxValue)
        _<- addBlockDefinitions(Seq(assignStmt))

        index_one <- arithmetic.arithmeticCapabilities.add(index, one)
        mark <- impParadigm.imperativeCapabilities.declareVar(names.mangle("i"), intType, Some(index_one))
        arr_index <- array.arrayCapabilities.get(arr, index)
        index_arr_index <- arithmetic.arithmeticCapabilities.add(index, arr_index)
        for_cond1 <- arithmetic.arithmeticCapabilities.lt(mark, index_arr_index)
        for_cond2 <- eqls.equalityCapabilities.areEqual(intType, mark, index_arr_index)
        for_cond <- booleans.booleanCapabilities.or(Seq(for_cond1, for_cond2))

        forStmt <- impParadigm.imperativeCapabilities.whileLoop(for_cond, for{
          arr_mark <- apply(recur_func, Seq(arr, mark))
          valExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("var"), intType, Some(arr_mark))
          innerIfNotCond <- eqls.equalityCapabilities.areEqual(intType, valExpr, maxValue)
          innerIfCond1 <- booleans.booleanCapabilities.not(innerIfNotCond)

          val_1 <- arithmetic.arithmeticCapabilities.add(valExpr, one)
          innerIfCond2 <- arithmetic.arithmeticCapabilities.lt(val_1, ansExpr)
          innerIfCond <- booleans.booleanCapabilities.and(Seq(innerIfCond1, innerIfCond2))
          innerIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(innerIfCond, for{

            assignStmt <- impParadigm.imperativeCapabilities.assignVar(ansExpr, val_1)
            _<-addBlockDefinitions(Seq(assignStmt))
          } yield (),
            Seq.empty
          )

          _<-addBlockDefinitions(Seq(innerIfStmt))

          mark_1 <- arithmetic.arithmeticCapabilities.add(mark, one)
          assignStmt <- impParadigm.imperativeCapabilities.assignVar(mark, mark_1)
          _<- addBlockDefinitions(Seq(assignStmt))
        } yield()
        )

        _<- addBlockDefinitions(Seq(forStmt))

      }yield (),
        Seq.empty
      )

      _<- addBlockDefinitions(Seq(if_Stmt))


    } yield(Some(ansExpr))
  }

  /*
  int minJumps(int[] arr) {

            int ans = minJumpsRecur(0, arr);

            if (ans == retrieveMax(arr))
                return -1;

            return ans;
        }
   */

  def minJumps(): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for{
      _ <- makeminJumpsSignature()
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      neg_one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -1)
      args <- getArguments()
      intType <- toTargetLanguageType(TypeRep.Int)
      (names1, tpes1, arr) = args.head
      retrieve_func <- find_method_recursive(names.mangle("retrieveMax"))
      recur_func <- find_method_recursive(names.mangle("minJumpsRecur"))
      maxValue <- apply(retrieve_func, Seq(arr))

      ansValue <- apply(recur_func, Seq(arr, zero))
      ansExpr <- impParadigm.imperativeCapabilities.declareVar(names.mangle("ans"), intType, Some(ansValue))

      if_cond <- eqls.equalityCapabilities.areEqual(intType, ansExpr, maxValue)
      ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(if_cond, for{
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(ansExpr, neg_one)
        _<- addBlockDefinitions(Seq(assignStmt))
      }yield (),
        Seq.empty)

      _<- addBlockDefinitions(Seq(ifStmt))
    } yield(Some(ansExpr))
  }





  /** Create the HouseRobber class with rob() method. */
  def makeClass(clazzName: String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("retrieveMax"), retireve_max())
        _ <- addMethod(names.mangle("minJumpsRecur"), minJumpRec())
        _<- addMethod(names.mangle("minJumps"), minJumps())

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
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

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
      _ <- makeClass("JumpTo")
      _ <- makeMainClass("Main")
    } yield ()
  }
}

object JumpToObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = JumpToObjectOrientedProvider {val paradigm: P}
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
  : JumpToObjectOrientedProvider.WithParadigm[base.type] =
    new JumpToObjectOrientedProvider {
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

