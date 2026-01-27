package org.combinators.dp

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, Strings}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.dp.Utility
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.model.{AdditionExpression, EqualExpression, FunctionExpression, IteratorExpression, LiteralInt, Model, SubproblemExpression, SubtractionExpression}

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait DPObjectOrientedProvider extends DPProvider with Utility {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val testName = names.mangle("TestSuite")
  lazy val helper   = names.mangle("helper")
  lazy val compute  = names.mangle("compute")
  lazy val memoName = names.mangle("memo")
  lazy val dpName   = names.mangle("dp")

  lazy val iName    = names.mangle("i")
  lazy val nName    = names.mangle("n")

  // will be set and then globally accessed
  var memo:Boolean = false

  // if not memo, then this will be defined and added
  lazy val resultVarName = names.mangle("result")

  def make_compute_method_signature(): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setParameters(Seq((nName, intType)))       // a bit of a hack. Should be able to extract from compute model
      _ <- setReturnType(intType)

    } yield ()
  }

  def outer_helper(model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      _ <- make_compute_method_signature()
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      _ <- process_inner_helper(model)
    } yield None
  }

  /**

   private int memo(int n) {
     if (this.memo.containsKey(n)) {
       return this.memo.get(n);
     }

     int result = helper(n);
     this.memo.put(n, result);
     return result;
   }

   */
  def memo_helper(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      _ <- make_compute_method_signature()
      args <- getArguments()
      intType <- toTargetLanguageType(TypeRep.Int)
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      memo_field <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("memo"))

      memo_ck <- ooParadigm.methodBodyCapabilities.getMember(memo_field, names.mangle("containsKey"))
      memo_cond_expr <- paradigm.methodBodyCapabilities.apply(memo_ck, Seq(args.head._3))
      check_if <- impParadigm.imperativeCapabilities.ifThenElse(memo_cond_expr, for {
        get_method <- ooParadigm.methodBodyCapabilities.getMember(memo_field, names.mangle("get"))
        get_call <- paradigm.methodBodyCapabilities.apply(get_method, Seq(args.head._3))
        stmt1 <- impParadigm.imperativeCapabilities.returnStmt(get_call)
        _ <- addBlockDefinitions(Seq(stmt1))
      } yield None, Seq.empty)
      _ <- addBlockDefinitions(Seq(check_if))

      helper_method <- ooParadigm.methodBodyCapabilities.getMember(self, helper)
      helper_expr <- paradigm.methodBodyCapabilities.apply(helper_method, Seq(args.head._3))
      result_var <- impParadigm.imperativeCapabilities.declareVar(resultVarName, intType, Some(helper_expr))

      self <- ooParadigm.methodBodyCapabilities.selfReference()
      memo_field <- ooParadigm.methodBodyCapabilities.getMember(self, memoName)
      put_method <- ooParadigm.methodBodyCapabilities.getMember(memo_field, names.mangle("put"))

      func_call <- paradigm.methodBodyCapabilities.apply(put_method, Seq(args.head._3, result_var))
      stmt1 <- impParadigm.imperativeCapabilities.liftExpression(func_call)
      _ <- addBlockDefinitions(Seq(stmt1))

    } yield Some(result_var)
  }

  /**
   * The workhorse for a top-down helper method that relies on recursion and base cases to do the work.
   * The model has a sequence of `cases` that may contain logical guard and an expression that is to be the reulst
   * for those cases.
   *
   * This code relies on a helper method and ensures that all base cases are resolved by return statements, and
   * the final "else" case is appended.
   *
   * The explore() method converts a model Expression into a CoGen expression. Must be sure to pass in memo
   * The expand() method converts a model Expression into a Return statement of that expression
   *
   * @return
   */
  def process_inner_helper(model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import AnyParadigm.syntax._

    val real_cases = model.cases.filter(p => p._1.isDefined) // MUST be at least one.
    val first_case = real_cases.head
    val tail_cases = real_cases.tail
    val elseCase = model.cases.filter(p => p._1.isEmpty) // MUST only be one. Not sure how I would check

    for {
      _ <- make_compute_method_signature()
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      inner <- explore(first_case._1.get, memoize=memo)

      all_rest <- forEach(tail_cases) { next_case =>
        for {
          next_cond <- explore(next_case._1.get, memoize=memo)
          next_exp <- explore(next_case._2, memoize=memo)
        } yield (next_cond, expand(next_exp))
      }

      ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
        // condition of first if
        inner
        ,
        // statements for that first if
        for {
          resexp <- explore(first_case._2, memoize=memo)
          av <- impParadigm.imperativeCapabilities.returnStmt(resexp)
          _ <- addBlockDefinitions(Seq(av))
        } yield None
        ,
        // collection of (condition, block) for all of the remaining cases
        all_rest
        ,
        // terminating 'else' takes the elseCase and adds it last
        Some(for {
          result_exp <- explore(elseCase.head._2, memoize=memo)
          av <- impParadigm.imperativeCapabilities.returnStmt(result_exp)
          _ <- addBlockDefinitions(Seq(av))
        } yield ())
      )

      _ <- addBlockDefinitions(Seq(ifstmt))
    } yield None
  }

  /**
   * Necessary wrapper method that inserts a return (expr) statement from the given expression. Needed for top-down, non-memo
   */
  private def expand(exp: Expression): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {
      av <- impParadigm.imperativeCapabilities.returnStmt(exp)
      _ <- addBlockDefinitions(Seq(av))
    } yield None
  }

  /** Needed when working bottom up. */
  private def expand_assign(dp_i:Expression, exp: Expression): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {
      av <- impParadigm.imperativeCapabilities.assignVar(dp_i, exp)
      _ <- addBlockDefinitions(Seq(av))
    } yield None
  }

  /**
   * Method creates a compute() method with no arguments that invokes helper method:
   *
   *   public Integer compute() {
   *     return this.helper(this.n);
   *   }
   */
  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      helperMethod <- ooParadigm.methodBodyCapabilities.getMember(self, helper)
      field <- ooParadigm.methodBodyCapabilities.getMember(self, nName)
      invocation <- apply(helperMethod, Seq(field))
    } yield Some(invocation)
  }

  def make_bottom_up_compute_method(model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    val real_cases = model.cases.filter(p => p._1.isDefined) // MUST be at least one.
    val first_case = real_cases.head
    val tail_cases = real_cases.tail
    val elseCase = model.cases.filter(p => p._1.isEmpty) // MUST only be one. Not sure how I would check

    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))

      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
      // seems to be different from Integer which occurs in MethodBodyContext
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      n <- ooParadigm.methodBodyCapabilities.getMember(self, nName)
      nplus1 <- arithmetic.arithmeticCapabilities.add(n, one)

      ivar <- impParadigm.imperativeCapabilities.declareVar(iName, intType, Some(zero))
      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      dp_i <- array.arrayCapabilities.get(dp, ivar)

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(nplus1), None)

      inner <- explore(first_case._1.get, bottomUp = Some((dp, Map("i" -> ivar))))

      all_rest <- forEach(tail_cases) { next_case =>
        for {
          next_cond <- explore(next_case._1.get, memoize = false, bottomUp = Some((dp, Map("i" -> ivar))))
          next_exp <- explore(next_case._2, memoize = false, bottomUp = Some((dp, Map("i" -> ivar))))
        } yield (next_cond, expand_assign(dp_i, next_exp))
      }

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))
      in_range <- arithmetic.arithmeticCapabilities.le(ivar, n)

      whileLoop <- impParadigm.imperativeCapabilities.whileLoop(in_range, for {
        ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
          // condition of first if
          inner
          ,
          // statements for that first if
          for {
            resexp <- explore(first_case._2, memoize = false, bottomUp = Some((dp, Map("i" -> ivar))))
            av <- impParadigm.imperativeCapabilities.assignVar(dp_i, resexp)
            _ <- addBlockDefinitions(Seq(av))
          } yield None
          ,
          // collection of (condition, block) for all of the remaining cases
          all_rest
          ,
          // terminating 'else' takes the elseCase and adds it last
          Some(for {
            result_exp <- explore(elseCase.head._2, memoize = false, bottomUp = Some((dp, Map("i" -> ivar))))
            av <- impParadigm.imperativeCapabilities.assignVar(dp_i, result_exp)
            _ <- addBlockDefinitions(Seq(av))
          } yield ())
        )

        ivarplusone <- arithmetic.arithmeticCapabilities.add(ivar, one)
        incr <- impParadigm.imperativeCapabilities.assignVar(ivar, ivarplusone)

        _ <- addBlockDefinitions(Seq(ifstmt, incr))
      } yield ())

      _ <- addBlockDefinitions(Seq(whileLoop))

      // return last element dp[n] because dp is 1 larger in size than n
      dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      dpn <- array.arrayCapabilities.get(dpexp, n)
      retstmt <- Command.lift(dpn)
    } yield Some(retstmt)
  }

  def make_memo_type(keyType:TypeRep, valueType:TypeRep): Generator[ConstructorContext, Type] = {
    import ooParadigm.constructorCapabilities._
    import genericsParadigm.constructorCapabilities._

    for {
      mapClass <- ooParadigm.constructorCapabilities.findClass(
        names.mangle("java"), names.mangle("util"), names.mangle("HashMap")
      )

      keyType <- toTargetLanguageType(keyType)
      valueType <- toTargetLanguageType(valueType)
      finalTpe <- applyType(mapClass, Seq(keyType, valueType))

    } yield finalTpe
  }

  /**
   * Constructor now takes the responsibility of taking the arguments to the problem. Takes
   * in a sequence of arguments, and auto-initializes all possible fields.
   */
  def create_bottom_up_constructor(args: Seq[(Name, Type)]): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    for {
      _ <- setParameters(args)
      real_args <- getArguments()

      _ <- forEach(real_args) { arg => for {
          _ <- initializeField(arg._1, arg._3)
        } yield ()
      }

//      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
//      nplus1 <- arithmetic.arithmeticCapabilities.add(real_args.head._3, one)
//      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))

//      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(nplus1), None)
//      self <- selfReference()

      // I CANNOT GET THIS TO WOK

      //dp <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("dp"))
      //_ <- initializeField(names.mangle("dp"), instantiated)

     // assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
     // _ <- addBlockDefinitions(Seq(assign_stmt))

    } yield ()
  }


  /**
   * Constructor now takes the responsibility of taking the arguments to the problem. Takes
   * in a sequence of arguments, and auto-initializes all possible fields.
   */
  def createConstructor(args: Seq[(Name, Type)]): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    for {
      _ <- setParameters(args)
      real_args <- getArguments()

      _ <- forEach(real_args) { arg => for {
          _ <- initializeField(arg._1, arg._3)
        } yield ()
      }

      _ <- if (memo) {
        for {
          tpe <- make_memo_type(TypeRep.Int, TypeRep.Int) // HACK
          obj <- instantiateObject(tpe, Seq.empty)
          _ <- initializeField(memoName, obj)
        } yield None
      } else {
        Command.skip[ConstructorContext]
      }
    } yield ()
  }

  def make_top_down(model:Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def makeMemo(keyType:TypeRep, valueType:TypeRep) : Generator[ClassContext, Unit] = {
      import classCapabilities._
      import genericsParadigm.classCapabilities._

      for {
        mapClass <- ooParadigm.classCapabilities.findClass(
          names.mangle("java"), names.mangle("util"), names.mangle("HashMap")
        )
        keyType <- toTargetLanguageType(keyType)
        valueType <- toTargetLanguageType(valueType)
        finalTpe <- applyType(mapClass, Seq(keyType, valueType))

        _ <- addField(memoName, finalTpe)
      } yield None
    }

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        intType <- toTargetLanguageType(TypeRep.Int)    // shouldn't be hard-coded: should be able to infer from model
        _ <- addField(nName, intType )
        _ <- if (memo) {
          makeMemo(TypeRep.Int, TypeRep.Int)
        } else {
          Command.skip[ClassContext]
        }
        _ <- addConstructor(createConstructor(Seq((nName, intType))))
        _ <- if (memo) {
          addMethod(memoName, memo_helper())
        } else {
          Command.skip[ClassContext]
        }

        _ <- addMethod(helper, outer_helper(model))
        _ <- addMethod(compute, make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
  }

  def make_bottom_up(model:Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        intType <- toTargetLanguageType(TypeRep.Int)    // shouldn't be hard-coded: should be able to infer from model
        arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))

        _ <- addField(nName, intType )
        _ <- addField(dpName, arrayType)   // this becomes "int" if I use arrayType

        _ <- addConstructor(create_bottom_up_constructor(Seq((nName, intType))))

        _ <- addMethod(compute, make_bottom_up_compute_method(model))
      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
  }


  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import eqls.equalityCapabilities._

    val tests = Seq(
      new DPExample("fib0", 0, 0, None),
      new DPExample("fib1", 1, 1, None),
      new DPExample("fib2", 2, 1, None),
      new DPExample("fib7", 7, 13, None),
      new DPExample("fib20", 20, 6765, None),
      new DPExample("fib40", 40, 102334155, None)
    )

    for {
      assert_statements <- forEach(tests) { example =>
        for {
          fibType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("Fibonacci"))
          n_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, example.example)
          sol <- ooParadigm.methodBodyCapabilities.instantiateObject(fibType, Seq(n_value))
          computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, compute)

          intType <- toTargetLanguageType(TypeRep.Int)
          fibn_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, example.solution)
          fib_actual <- apply(computeMethod, Seq.empty)
          asserteq_fib <- asserts.assertionCapabilities.assertEquals(intType, fib_actual, fibn_value)

        } yield asserteq_fib
      }
    } yield assert_statements
  }

  def makeTestCase(clazzName:String): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTestCase(), names.mangle(clazzName))
    } yield ()
  }

  def implement(model:Model, option:GenerationOption): Generator[ProjectContext, Unit] = {

    // handle Top/Bottom and properly set memo when TD
    var isTopDown = false
    option match {
      case td:TopDown =>
        memo = td.memo
        isTopDown = true

      case _:BottomUp =>
        isTopDown = false
    }

    for {
      _ <- if (isTopDown) {
        make_top_down(model)
      } else {
        make_bottom_up(model)
      }

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
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   stringsIn: Strings.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],oo: ObjectOriented.WithBase[base.type],
   parametricPolymorphism: ParametricPolymorphism.WithBase[base.type])
  (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): DPObjectOrientedProvider.WithParadigm[base.type] =
    new DPObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: oo.type = oo
      override val polymorphics: parametricPolymorphism.type = parametricPolymorphism
      override val genericsParadigm: generics.type = generics
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val strings: Strings.WithBase[base.MethodBodyContext, paradigm.type] = stringsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
    }
}
