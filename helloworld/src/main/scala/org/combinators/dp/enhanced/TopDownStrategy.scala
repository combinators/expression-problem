package org.combinators.dp.enhanced

import org.combinators.dp.Utility
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi._
import org.combinators.ep.generator.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.{Command, NameProvider}
import org.combinators.model.{Definition, DefinitionStatement, EnhancedModel, ExpressionDefinition, ExpressionStatement, IfThenElseDefinition, LiteralBoolean, LiteralInt, LiteralString, MinRangeDefinition, SubproblemInvocation}

/**
 * Concepts necessary to realize top-down solutions
 */

trait TopDownStrategy extends Utility with EnhancedUtility {
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

  import ooParadigm._
  import paradigm._
  import syntax._

  // Definition of the name of the helper method
  val helperName:Name
  val computeName:Name

  lazy val memoName       = names.mangle("memo")
  lazy val keyName        = names.mangle("key")
  lazy val pairName       = names.mangle("pair")

  lazy val computedResult = names.mangle("computed_result")

  // is provided by the DP common provider -- neither a topDown or a bottomUp concept
  def make_compute_method(model:EnhancedModel): Generator[paradigm.MethodBodyContext, Option[Expression]]


  /**
       private int memo(ARGUMENTS) {
         int key = pair(ARGUMENTS)
         if (this.memo.containsKey(n)) {
           return this.memo.get(n);
         }

         int result = helper(n);
         this.memo.put(n, result);
         return result;
       }
   */
  def memo_helper_body(model:EnhancedModel): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      // need to convert into a KEY method. MUST have at least one argument
      args <- getArguments()
      allExpressions <- forEach(args) { arg => for {
          _ <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -97)     // There needs to be at least one of these before "=" next
          expr = arg._3
        } yield expr
      }

      intType <- toTargetLanguageType(TypeRep.Int)
      helperType <- helper_method_type(model)
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      memo_field <- ooParadigm.methodBodyCapabilities.getMember(self, memoName)

      pair_func <- ooParadigm.methodBodyCapabilities.getMember(self, pairName)

      key_var <- if (args.length == 1) {
        impParadigm.imperativeCapabilities.declareVar(keyName, intType, Some(args.head._3))
      } else if (args.length == 2) {
        for {
          pair_expr <- paradigm.methodBodyCapabilities.apply(pair_func, allExpressions)
        } yield pair_expr
      } else {
        // fold everything in, after calling pair() on the final two parameters
        val base_expr = paradigm.methodBodyCapabilities.apply(pair_func, allExpressions.slice(allExpressions.length - 2, allExpressions.length))
        allExpressions.slice(0, allExpressions.length - 2).foldRight(base_expr)((acc, index) => for {
          expr <- index
          expanded <- paradigm.methodBodyCapabilities.apply(pair_func, Seq(acc, expr))
        } yield expanded)
      }

      memo_ck <- ooParadigm.methodBodyCapabilities.getMember(memo_field, names.mangle("containsKey"))

      memo_cond_expr <- paradigm.methodBodyCapabilities.apply(memo_ck, Seq(key_var))
      check_if <- impParadigm.imperativeCapabilities.ifThenElse(memo_cond_expr, for {
        get_method <- ooParadigm.methodBodyCapabilities.getMember(memo_field, names.mangle("get"))
        get_call <- paradigm.methodBodyCapabilities.apply(get_method, Seq(key_var))
        stmt1 <- impParadigm.imperativeCapabilities.returnStmt(get_call)
        _ <- addBlockDefinitions(Seq(stmt1))
      } yield None, Seq.empty)
      _ <- addBlockDefinitions(Seq(check_if))

      helper_method <- ooParadigm.methodBodyCapabilities.getMember(self, helperName)

      helper_expr <- paradigm.methodBodyCapabilities.apply(helper_method, allExpressions)
      result_var <- impParadigm.imperativeCapabilities.declareVar(computedResult, helperType, Some(helper_expr))

      self <- ooParadigm.methodBodyCapabilities.selfReference()
      memo_field <- ooParadigm.methodBodyCapabilities.getMember(self, memoName)
      put_method <- ooParadigm.methodBodyCapabilities.getMember(memo_field, names.mangle("put"))

      func_call <- paradigm.methodBodyCapabilities.apply(put_method, Seq(key_var, result_var))
      stmt1 <- impParadigm.imperativeCapabilities.liftExpression(func_call)
      _ <- addBlockDefinitions(Seq(stmt1))

    } yield Some(result_var)
  }

  /**
   * Create the MemoType, which is always HashMap<Integer,Integer> because the solution to a DP is an integer, and
   * you can convert the subproblem arguments into an Integer using Cantor's pairing function.
   */
  def make_memo_type(keyType:Type, valueType:Type): Generator[ConstructorContext, Type] = {
    import genericsParadigm.constructorCapabilities._
    import ooParadigm.constructorCapabilities._

    for {
      mapClass <- ooParadigm.constructorCapabilities.findClass(
        names.mangle("java"), names.mangle("util"), names.mangle("HashMap")
      )

      finalTpe <- applyType(mapClass, Seq(keyType, valueType))

    } yield finalTpe
  }

  /**
   * Constructor now takes the responsibility of taking the arguments to the problem. Takes
   * in a sequence of arguments, and auto-initializes all possible fields.
   */
  def createConstructor(model:EnhancedModel, useMemo:Boolean, args: Seq[(Name, Type)]): Generator[ConstructorContext, Unit] = {
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
          intType <- toTargetLanguageType(TypeRep.Int)
          helperType <- helper_method_type_in_constructor(model)
          tpe <- make_memo_type(intType, helperType)         // Key is key() result, and Value is DP-int solution
          obj <- instantiateObject(tpe, Seq.empty)
          _ <- initializeField(memoName, obj)
        } yield None
      } else {
        Command.skip[ConstructorContext]
      }
    } yield ()
  }

  def make_top_down(useMemo:Boolean, model:EnhancedModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def makeMemo(keyType:Type, valueType:Type) : Generator[ClassContext, Unit] = {
      import classCapabilities._
      import genericsParadigm.classCapabilities._

      for {
        mapClass <- ooParadigm.classCapabilities.findClass(
          names.mangle("java"), names.mangle("util"), names.mangle("HashMap")
        )
        finalTpe <- applyType(mapClass, Seq(keyType, valueType))

        _ <- addField(memoName, finalTpe)
      } yield None
    }

    def create_memo_helper(): Generator[MethodBodyContext, Option[Expression]] = {
      import paradigm.methodBodyCapabilities._
      for {
        _ <- symbol_table_from_solution(model.solution)  // should be done outside
        retType <- helper_method_type(model)
        _ <- setReturnType(retType)
        res <- memo_helper_body(model)
      } yield res
    }

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- forEach(model.input) { bexpr => for {
            tpe <- map_type_in_class(bexpr.argType)
            _ <- addField(names.mangle(bexpr.name), tpe)
          } yield ()
        }

       _ <- if (useMemo) {
         for {
           intType <- toTargetLanguageType(TypeRep.Int)  // key will always be an IntType since that is the cantor pairing
           returnType <- helper_method_type_in_class(model)
          _ <- makeMemo(intType, returnType)
         } yield ()
        } else {
          Command.skip[ClassContext]
        }
        constArgs <- forEach(model.input) { bexpr =>
          for {
            tpe <- map_type_in_class(bexpr.argType)
          } yield (names.mangle(bexpr.name), tpe)
        }

        _ <- addConstructor(createConstructor(model, useMemo, constArgs))   // FIX HACK

        _ <- if (useMemo) {
          for {
            _ <- addMethod(memoName, create_memo_helper())
            _ <- addMethod(pairName, pair_helper())
          } yield ()
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
    val s:Seq[Statement] = Seq.empty
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
  def process_inner_helper(useMemo:Boolean, model:EnhancedModel): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import AnyParadigm.syntax._
    import paradigm.methodBodyCapabilities._

    def exploreReturns(defs:DefinitionStatement, symbolTable: Map[String,Expression], memoize:Boolean = false) : Generator[paradigm.MethodBodyContext, Seq[Statement]] = {
      defs match {
        case es:ExpressionStatement => for {
          e <- explore(es.expr, memoize = useMemo, symbolTable = symbolTable)
          av <- impParadigm.imperativeCapabilities.returnStmt(e)
        } yield Seq(av)

        case _ => ???
      }
    }

    def generate (defn:Definition, symbolTable: Map[String,Expression], memoize:Boolean = false) : Generator[paradigm.MethodBodyContext, Seq[Statement]] = {
      import paradigm.methodBodyCapabilities._
      import ooParadigm.methodBodyCapabilities._
      import AnyParadigm.syntax._

      defn match {
        case ed:ExpressionDefinition => for {
          expr <- explore(ed.expr, symbolTable = symbolTable, memoize=memoize)
          retval <- impParadigm.imperativeCapabilities.returnStmt(expr)
        } yield Seq(retval)

        case ite:IfThenElseDefinition => for {
          inner <- explore(ite.condition, memoize = false, symbolTable = symbolTable)
          ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
            // condition of first if
            inner
            ,
            // statements for that first if
            for {
              stmts <- exploreReturns(ite.result, symbolTable = symbolTable, memoize=memoize)
              _ <- addBlockDefinitions(stmts)
            } yield ()
            ,
            // collection of (condition, block) for all remaining cases
            Seq.empty
            ,
            // terminating 'else' takes the elseCase and adds it last
            Some(for {
              stmts <- generate(ite.elseExpression, symbolTable = symbolTable, memoize = useMemo)
              _ <- addBlockDefinitions(stmts)
            } yield ())
          )
        } yield Seq(ifstmt)

        case ds:MinRangeDefinition => for {
          one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, scala.Int.MaxValue)
          intType <- toTargetLanguageType(TypeRep.Int)   // hack
          minVarName = names.mangle("min")
          minVar <- impParadigm.imperativeCapabilities.declareVar(minVarName, intType, Some(one))   //
          kStart <- explore(ds.inclusiveStart, symbolTable = symbolTable, memoize = memoize)
          kVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(ds.variable.variable), intType, Some(kStart))   //

          resultVarName = names.mangle("result")
          resultVar <- impParadigm.imperativeCapabilities.declareVar(resultVarName, intType, None)
          addedSymbolTable = symbolTable + ("min" -> minVar) + ("k" -> kVar) + ("result" -> resultVar)

          minCond <- arithmetic.arithmeticCapabilities.lt(resultVar, minVar)
          guardCondition <- explore(ds.guardContinue, symbolTable = addedSymbolTable, memoize = memoize)
          whilestmt <- impParadigm.imperativeCapabilities.whileLoop(guardCondition, for {
            neg99 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -99)

            resultExpr <- explore(ds.subproblemExpression, memoize = memoize, symbolTable = addedSymbolTable)
            assignResult <- impParadigm.imperativeCapabilities.assignVar(resultVar, resultExpr)

            // record minimum
            update <- impParadigm.imperativeCapabilities.ifThenElse(minCond, for {
              updateResult <- impParadigm.imperativeCapabilities.assignVar(minVar, resultVar)
              _ <- addBlockDefinitions(Seq(updateResult))
              // here is where one could store deecisions
            } yield (), Seq.empty, None)


            advExpr <- explore(ds.advance, memoize=memoize, symbolTable=addedSymbolTable)
            kadv <- impParadigm.imperativeCapabilities.assignVar(kVar, advExpr)
            _ <- addBlockDefinitions(Seq(assignResult, update, kadv))
          } yield ())

          returnResult <- impParadigm.imperativeCapabilities.returnStmt(minVar)
        } yield Seq(whilestmt, returnResult)

        case _ => ???
      }

    }

    // could possibly have a definition that has NONE as the guard. None for now.

    for {
      symbolTable <- symbol_table_from_solution(model.solution)
      ifstmt <- generate(model.definition, symbolTable, memoize=useMemo)
      _ <- addBlockDefinitions(ifstmt)

    } yield None
  }

  /**
   * Creates function using parameters from model and returns int:
   *
   *     int SOMEFUNCTION (ARGS)
   *
   * where ARGS represents the model (i.e., (("n", Int)) for Fibonacci and (("s1", String), ("s2", String)) for LCS
   */
  def symbol_table_from_solution(solution:SubproblemInvocation): Generator[paradigm.MethodBodyContext, Map[String, Expression]] = {
    import paradigm.methodBodyCapabilities._

    // Type of helper method param is always an integer to refer to earlier subproblem
    for {
      params <- forEach(solution.parameters.toSeq) { pair => for {
        argType <- toTargetLanguageType(TypeRep.Int)      // Always will be int since subproblems are ordered
        argName = names.mangle(pair._1)             // use pre-selected iterator
      } yield (argName, argType)
      }
      _ <- setParameters(params)
      args <- getArguments()

      mapargs <- forEach(solution.parameters.toSeq zip args) { pair =>
        for {
          argType <- toTargetLanguageType(TypeRep.Int)   // needed syntactically, and will be ignored.
          argExpr = pair._2._3
          argName = pair._1._1
      } yield (argName, argExpr)
      }

    } yield mapargs.toMap
  }

  def outer_helper(useMemo: Boolean, model:EnhancedModel): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    for {
      symbolTable <- symbol_table_from_solution(model.solution)

      realType <- return_type_based_on_model(model)
      _ <- setReturnType(realType)

      _ <- process_inner_helper(useMemo, model)
    } yield None
  }


}