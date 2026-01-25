package org.combinators.dp

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
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
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val message:String = "message"
  lazy val main:String = "main"
  lazy val testName = names.mangle("TestSuite")
  lazy val helper = names.mangle("helper")
  lazy val compute = names.mangle("compute")
  lazy val memoName = names.mangle("memo")

  // will be set and then globally accessed
  var memo:Boolean = false

  // if not memo, then this will be defined and added
  lazy val resultVarName = names.mangle("result")

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
      _ <- setParameters(Seq((names.mangle("n"), intType)))
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
   * Necessary wrapper method that inserts a return (expr) statement from the given expression.
   *
   * @return
   */
  private def expand(exp: Expression): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {
      av <- impParadigm.imperativeCapabilities.returnStmt(exp)
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
      field <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("n"))
      invocation <- apply(helperMethod, Seq(field))
    } yield Some(invocation)
  }

  def makeMemoType(keyType:TypeRep, valueType:TypeRep): Generator[ConstructorContext, Type] = {
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
          tpe <- makeMemoType(TypeRep.Int, TypeRep.Int) // HACK
          obj <- instantiateObject(tpe, Seq.empty)
          _ <- initializeField(names.mangle("memo"), obj)
        } yield None
      } else {
        Command.skip[ConstructorContext]
      }
    } yield ()
  }

  def makeTopDown(model:Model): Generator[ProjectContext, Unit] = {
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

        _ <- addField(names.mangle("memo"), finalTpe)
      } yield None
    }

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        intType <- toTargetLanguageType(TypeRep.Int)    // shouldn't be hard-coded: should be able to infer from model
        _ <- addField(names.mangle("n"), intType )
        _ <- if (memo) {
          makeMemo(TypeRep.Int, TypeRep.Int)
        } else {
          Command.skip[ClassContext]
        }
        _ <- addConstructor(createConstructor(Seq((names.mangle("n"), intType))))
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

  def implement(model:Model): Generator[ProjectContext, Unit] = {
    memo = true
    for {
      _ <- makeTopDown(model)
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
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
    }
}
