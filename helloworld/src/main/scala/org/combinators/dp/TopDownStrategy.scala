package org.combinators.dp

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.{Command, NameProvider}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Equality, Strings}
import org.combinators.model.{AdditionExpression, ArgumentType, EnhancedModel, EqualExpression, InputExpression, IteratorExpression, LiteralInt, Model, StringLengthExpression, SubproblemExpression, SubtractionExpression}

/**
 * Concepts necessary to realize top-down solutions
 */

trait TopDownStrategy extends Utility {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]

  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  // Definition of the name of the helper method
  val helperName:Name
  val computeName:Name

  lazy val memoName       = names.mangle("memo")
  lazy val keyName        = names.mangle("key")
  lazy val pairName       = names.mangle("pair")

  lazy val computedResult = names.mangle("computed_result")

  // is provided by the DP common provider -- neither a topDown or a bottomUp concept
  def make_compute_method(model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]]

  def create_key(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      args <- getArguments()

    } yield None
  }

  /**
       private int memo(ARGUMENTS) {
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
      // need to convert into a KEY method. MUST have at least one argument
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

      helper_method <- ooParadigm.methodBodyCapabilities.getMember(self, helperName)
      helper_expr <- paradigm.methodBodyCapabilities.apply(helper_method, Seq(args.head._3))
      result_var <- impParadigm.imperativeCapabilities.declareVar(computedResult, intType, Some(helper_expr))

      self <- ooParadigm.methodBodyCapabilities.selfReference()
      memo_field <- ooParadigm.methodBodyCapabilities.getMember(self, memoName)
      put_method <- ooParadigm.methodBodyCapabilities.getMember(memo_field, names.mangle("put"))

      func_call <- paradigm.methodBodyCapabilities.apply(put_method, Seq(args.head._3, result_var))
      stmt1 <- impParadigm.imperativeCapabilities.liftExpression(func_call)
      _ <- addBlockDefinitions(Seq(stmt1))

    } yield Some(result_var)
  }

  /**
   * Create the MemoType, which is always HashMap<Integer,Integer> because the solution to a DP is an integer, and
   * you can convert the subproblem arguments into an Integer using Cantor's pairing function.
   */
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
  def createConstructor(useMemo:Boolean, args: Seq[(Name, Type)]): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    for {
      _ <- setParameters(args)
      real_args <- getArguments()

      _ <- forEach(real_args) { arg => for {
        _ <- initializeField(arg._1, arg._3)
      } yield ()
      }

      _ <- if (useMemo) {
        for {
          tpe <- make_memo_type(TypeRep.Int, TypeRep.Int)         // Key is key() result, and Value is DP-int solution
          obj <- instantiateObject(tpe, Seq.empty)
          _ <- initializeField(memoName, obj)
        } yield None
      } else {
        Command.skip[ConstructorContext]
      }
    } yield ()
  }

  def make_top_down(useMemo:Boolean, model:Model): Generator[ProjectContext, Unit] = {
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

    def create_memo_helper(): Generator[MethodBodyContext, Option[Expression]] = {
      for {
        _ <- make_helper_method_signature(model.bounds)  // should be done outside
        res <- memo_helper()
      } yield res
    }

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        intType <- toTargetLanguageType(TypeRep.Int)    // shouldn't be hard-coded: should be able to infer from model

        _ <- forEach(model.bounds) { bexpr => for {
            tpe <- map_type_in_class(bexpr.argType)
            _ <- addField(names.mangle(bexpr.name), tpe)
          } yield ()
        }

        _ <- if (useMemo) {
          makeMemo(TypeRep.Int, TypeRep.Int)
        } else {
          Command.skip[ClassContext]
        }
        constArgs <- forEach(model.bounds) { bexpr =>
          for {
            tpe <- map_type_in_class(bexpr.argType)
          } yield (names.mangle(bexpr.name), tpe)
        }

        _ <- addConstructor(createConstructor(useMemo, constArgs))   // FIX HACK

        _ <- if (useMemo) {
          addMethod(keyName, create_key())
          addMethod(memoName, create_memo_helper())
          addMethod(pairName, pair_helper())
        } else {
          Command.skip[ClassContext]
        }

        _ <- addMethod(helperName, outer_helper(useMemo, model))
        _ <- addMethod(computeName, make_compute_method(model))
      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
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
  def process_inner_helper(useMemo:Boolean, model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import AnyParadigm.syntax._

    val real_cases = model.cases.filter(p => p._1.isDefined) // MUST be at least one.
    val first_case = real_cases.head
    val tail_cases = real_cases.tail
    val elseCase = model.cases.filter(p => p._1.isEmpty) // MUST only be one. Not sure how I would check

    for {
      _ <- make_helper_method_signature(model.bounds)
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      inner <- explore(first_case._1.get, memoize=useMemo, symbolTable = Map.empty)

      all_rest <- forEach(tail_cases) { next_case =>
        for {
          next_cond <- explore(next_case._1.get, memoize=useMemo, symbolTable = Map.empty)
          next_exp <- explore(next_case._2, memoize=useMemo, symbolTable = Map.empty)
        } yield (next_cond, expand(next_exp))
      }

      ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
        // condition of first if
        inner
        ,
        // statements for that first if
        for {
          resexp <- explore(first_case._2, memoize=useMemo, symbolTable = Map.empty)
          av <- impParadigm.imperativeCapabilities.returnStmt(resexp)
          _ <- addBlockDefinitions(Seq(av))
        } yield None
        ,
        // collection of (condition, block) for all remaining cases
        all_rest
        ,
        // terminating 'else' takes the elseCase and adds it last
        Some(for {
          result_exp <- explore(elseCase.head._2, memoize=useMemo, symbolTable = Map.empty)
          av <- impParadigm.imperativeCapabilities.returnStmt(result_exp)
          _ <- addBlockDefinitions(Seq(av))
        } yield ())
      )

      _ <- addBlockDefinitions(Seq(ifstmt))
    } yield None
  }

  def outer_helper(useMemo: Boolean, model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      _ <- make_helper_method_signature(model.bounds)
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      _ <- process_inner_helper(useMemo, model)
    } yield None
  }



}