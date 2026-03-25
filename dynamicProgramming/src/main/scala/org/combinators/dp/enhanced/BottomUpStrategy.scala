package org.combinators.dp.enhanced

import org.combinators.dp.Utility
import org.combinators.cogen.TypeRep
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi._
import org.combinators.cogen.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.cogen.NameProvider
import org.combinators.models._

/**
 * Concepts necessary to realize top-down solutions
 */

trait BottomUpStrategy extends Utility with EnhancedUtility {
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

  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val iName      = names.mangle("i")
  lazy val nName      = names.mangle("n")
  lazy val dpName     = names.mangle("dp")

  // will need to be expanded to depth-10 or something
  def arTypes(model: EnhancedModel): TypeRep = {

    val params = model.mode match {
      case ut:UpperTriangle => ut.params
      case _ => model.solution.order
    }

    // was model.input.length
    model.subproblemType match {
      case _:IntegerType =>
        params.length match {
          case 1 => TypeRep.Array(TypeRep.Int)
          case 2 => TypeRep.Array(TypeRep.Array(TypeRep.Int))
          case 3 => TypeRep.Array(TypeRep.Array(TypeRep.Array(TypeRep.Int)))
          case _ =>  ???
        }

      case _:CharType =>
        params.length match {
          case 1 => TypeRep.Array(TypeRep.Char)
          case 2 => TypeRep.Array(TypeRep.Array(TypeRep.Char))
          case 3 => TypeRep.Array(TypeRep.Array(TypeRep.Array(TypeRep.Char)))
          case _ =>  ???
        }

      case _:BooleanType =>
        params.length match {
          case 1 => TypeRep.Array(TypeRep.Boolean)
          case 2 => TypeRep.Array(TypeRep.Array(TypeRep.Boolean))
          case 3 => TypeRep.Array(TypeRep.Array(TypeRep.Array(TypeRep.Boolean)))
          case _ =>  ???
        }

      case _ => ???
    }
  }

  def make_bottom_up_compute_method_recursive(model:EnhancedModel, order:Seq[String]): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    case class TraversalInformation(
        statements:Seq[Statement],
        highs:Seq[Expression],
        symbols:Map[String,Expression],
        computedDpAccess:Expression,
        dpAccess:Expression)

    // return (Seq[statement] containing whileloop(s) and then seq of high vars, to be used by instantiation
    def traverse_inwards(level:Int, dpAccess:Expression, symbolTable:Map[String, Expression]): Generator[MethodBodyContext, TraversalInformation] = {
      if (level == order.length) {
        for {
          self <- ooParadigm.methodBodyCapabilities.selfReference()
          dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

          // This handles more advances cases when there are mappers
          real_access <- order.foldLeft(ooParadigm.methodBodyCapabilities.getMember(self, dpName)) ( (state, level_var) =>
            for {
              current_dp <- state
              next_expr <- explore(model.find_map(level_var), bottomUp=Some(dp), symbolTable=symbolTable, memoize=false)
              dp_next <- array.arrayCapabilities.get(current_dp, Seq(next_expr))
            } yield dp_next
          )

          av <- generate(dp, real_access, model.definition, symbolTable=symbolTable)
         } yield TraversalInformation(av, Seq.empty, symbolTable, real_access, dpAccess)
      } else {
        for {
          intType <- toTargetLanguageType(TypeRep.Int)

          self <- ooParadigm.methodBodyCapabilities.selfReference()
          dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

          level_low <- explore(model.find(order(level)).low, bottomUp=Some(dp), symbolTable=symbolTable, memoize=false)
          level_high <- explore(model.find(order(level)).high, bottomUp=Some(dp), symbolTable=symbolTable, memoize=false)
          level_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(level)), intType, None)
          level_map = symbolTable + (order(level) -> level_var)

          expr <- explore(model.find(order(level)), bottomUp=Some(dp), symbolTable=level_map)
          dp_next <- array.arrayCapabilities.get(dpAccess, Seq(expr))

          one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
          traversalInfo <- traverse_inwards(level + 1, dp_next, level_map)

          level_condition <- explore(model.find(order(level)).in_range, bottomUp=Some(dp), symbolTable=level_map, memoize=false)

          // reset counter variable
          loop_var_decl <- impParadigm.imperativeCapabilities.assignVar(level_var, level_low)
          level_whileLoop <- impParadigm.imperativeCapabilities.whileLoop(level_condition, for {
            _ <- addBlockDefinitions(traversalInfo.statements)

            level_var_plusone <- arithmetic.arithmeticCapabilities.add(level_var, one)
            incr_inner <- impParadigm.imperativeCapabilities.assignVar(level_var, level_var_plusone)

            _ <- addBlockDefinitions(Seq(incr_inner))
          } yield ())
        } yield TraversalInformation(Seq(loop_var_decl,level_whileLoop), level_high +: traversalInfo.highs, traversalInfo.symbols, traversalInfo.computedDpAccess, traversalInfo.dpAccess)
      }
    }

    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      theType <- return_type_based_on_model(model)
      _ <- setReturnType(theType)

      // ONLY ONE HERE
      arrayType <- toTargetLanguageType(arTypes(model))

      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
      // seems to be different from Integer which occurs in MethodBodyContext
      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

      traversalInfo <- traverse_inwards(0, dp, Map.empty)
      instantiated <- array.arrayCapabilities.create(theType, traversalInfo.highs, None)   // in new situation, only type of element, NOT full [][][] type
      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)

      _ <- addBlockDefinitions(Seq(assign_stmt))
      _ <- addBlockDefinitions(traversalInfo.statements)

      // when this is a ReturnExpression, essentially dpAccess is ignored since drawn solely from model.answer
      // the model would be responsible for choosing computedAccess or dpAccess. for now, leave as future work
      choice = traversalInfo.dpAccess
      av <- generate(dp, choice, model.answer, symbolTable=traversalInfo.symbols)
      _ <- addBlockDefinitions(av)

    } yield None
  }


  def report(str:String) : Generator[paradigm.MethodBodyContext, Unit] = {
    println(str)
    for  {
      ne77 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -77)
    } yield ()
  }

  def exploreExpr(dp:Expression, defs:DefinitionStatement, symbolTable: Map[String,Expression]) : Generator[paradigm.MethodBodyContext, Expression] = {
    defs match {
      case es:ExpressionStatement => for {
        e <- explore(es.expr, memoize = false, symbolTable = symbolTable, bottomUp=Some(dp))
      } yield e

      case _ => ???
    }
  }

  def generate (dp:Expression, dpij:Expression, defn:Definition, symbolTable: Map[String,Expression]) : Generator[paradigm.MethodBodyContext, Seq[Statement]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import AnyParadigm.syntax._

    defn match {
      case ed: ExpressionDefinition => for {
        expr <- explore(ed.expr, symbolTable = symbolTable, bottomUp = Some(dp))
        assigned <- impParadigm.imperativeCapabilities.assignVar(dpij, expr)
      } yield Seq(assigned)

      case ed: ReturnExpressionDefinition => for {
        expr <- explore(ed.expr, symbolTable = symbolTable, bottomUp = Some(dp))
        assigned <- impParadigm.imperativeCapabilities.returnStmt(expr)
      } yield Seq(assigned)

      case ite: IfThenElseDefinition => for {
        inner <- explore(ite.condition, symbolTable = symbolTable, bottomUp = Some(dp))
        ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
          // condition of first if
          inner
          ,
          // statements for that first if
          for {
            expr <- exploreExpr(dp, ite.result, symbolTable = symbolTable)
            assigned <- impParadigm.imperativeCapabilities.assignVar(dpij, expr)
            _ <- addBlockDefinitions(Seq(assigned))
          } yield ()
          ,
          // collection of (condition, block) for all remaining cases
          Seq.empty
          ,
          // terminating 'else' takes the elseCase and adds it last
          Some(for {
            stmts <- generate(dp, dpij, ite.elseExpression, symbolTable = symbolTable)
            _ <- addBlockDefinitions(stmts)
          } yield ())
        )
      } yield Seq(ifstmt)

      case ds: MinRangeDefinition => for {
        one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, scala.Int.MaxValue)
        intType <- toTargetLanguageType(TypeRep.Int) // hack
        minVarName = names.mangle("min")
        minVar <- impParadigm.imperativeCapabilities.declareVar(minVarName, intType, Some(one))
        kStart <- explore(ds.inclusiveStart, symbolTable = symbolTable, bottomUp = Some(dp))
        kVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(ds.variable), intType, Some(kStart))

        resultVarName = names.mangle("result")
        resultVar <- impParadigm.imperativeCapabilities.declareVar(resultVarName, intType, None)
        addedSymbolTable = symbolTable + ("min" -> minVar) + ("k" -> kVar) + ("result" -> resultVar)

        minCond <- arithmetic.arithmeticCapabilities.lt(resultVar, minVar)
        guardCondition <- explore(ds.guardContinue, symbolTable = addedSymbolTable, bottomUp = Some(dp))
        whilestmt <- impParadigm.imperativeCapabilities.whileLoop(guardCondition, for {
          neg99 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -99)

          resultExpr <- explore(ds.subproblemExpression, symbolTable = addedSymbolTable, bottomUp = Some(dp))
          assignResult <- impParadigm.imperativeCapabilities.assignVar(resultVar, resultExpr)

          // record minimum
          update <- impParadigm.imperativeCapabilities.ifThenElse(minCond, for {
            updateResult <- impParadigm.imperativeCapabilities.assignVar(minVar, resultVar)
            _ <- addBlockDefinitions(Seq(updateResult))
            // here is where one could store decisions
          } yield (), Seq.empty, None)

          advExpr <- explore(ds.advance, symbolTable = addedSymbolTable, bottomUp = Some(dp))
          kadv <- impParadigm.imperativeCapabilities.assignVar(kVar, advExpr)
          _ <- addBlockDefinitions(Seq(assignResult, update, kadv))
        } yield ())

        assigned <- impParadigm.imperativeCapabilities.assignVar(dpij, minVar)
      } yield Seq(whilestmt, assigned)

      case ds: MaxRangeDefinition => for {
        one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, scala.Int.MaxValue)
        intType <- toTargetLanguageType(TypeRep.Int) // hack
        maxVarName = names.mangle("max")
        maxVar <- impParadigm.imperativeCapabilities.declareVar(maxVarName, intType, Some(one))
        kStart <- explore(ds.inclusiveStart, symbolTable = symbolTable, bottomUp = Some(dp))
        kVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(ds.variable), intType, Some(kStart))

        resultVarName = names.mangle("result")
        resultVar <- impParadigm.imperativeCapabilities.declareVar(resultVarName, intType, None)
        addedSymbolTable = symbolTable + ("max" -> maxVar) + ("k" -> kVar) + ("result" -> resultVar)

        maxCond <- arithmetic.arithmeticCapabilities.lt(maxVar, resultVar)
        guardCondition <- explore(ds.guardContinue, symbolTable = addedSymbolTable, bottomUp = Some(dp))
        whilestmt <- impParadigm.imperativeCapabilities.whileLoop(guardCondition, for {

          resultExpr <- explore(ds.subproblemExpression, symbolTable = addedSymbolTable, bottomUp = Some(dp))
          assignResult <- impParadigm.imperativeCapabilities.assignVar(resultVar, resultExpr)

          // record minimum
          update <- impParadigm.imperativeCapabilities.ifThenElse(maxCond, for {
            updateResult <- impParadigm.imperativeCapabilities.assignVar(maxVar, resultVar)
            _ <- addBlockDefinitions(Seq(updateResult))
            // here is where one could store decisions
          } yield (), Seq.empty, None)

          advExpr <- explore(ds.advance, symbolTable = addedSymbolTable, bottomUp = Some(dp))
          kadv <- impParadigm.imperativeCapabilities.assignVar(kVar, advExpr)
          _ <- addBlockDefinitions(Seq(assignResult, update, kadv))
        } yield ())

        assigned <- impParadigm.imperativeCapabilities.assignVar(dpij, maxVar)
      } yield Seq(whilestmt, assigned)

      case sd: SumDefinition => for {
        intType <- toTargetLanguageType(TypeRep.Int)
        kStart <- explore(sd.inclusiveStart, symbolTable = symbolTable, bottomUp = Some(dp))
        kVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.variable), intType, Some(kStart))

        guardCondition <- explore(sd.guardContinue, symbolTable = symbolTable ++ Map(sd.variable -> kVar), bottomUp = Some(dp))
        whilestmt <- impParadigm.imperativeCapabilities.whileLoop(guardCondition, for {

          resultExpr <- explore(sd.subproblemExpression, symbolTable = symbolTable ++ Map(sd.variable -> kVar), bottomUp = Some(dp))
          additive <- arithmetic.arithmeticCapabilities.add(dpij, resultExpr)
          assignResult <- impParadigm.imperativeCapabilities.assignVar(dpij, additive)

          advExpr <- explore(sd.advance, symbolTable = symbolTable ++ Map(sd.variable -> kVar), bottomUp = Some(dp))
          kadv <- impParadigm.imperativeCapabilities.assignVar(kVar, advExpr)
          _ <- addBlockDefinitions(Seq(assignResult, kadv))

        } yield ())

      } yield Seq(whilestmt)

      case sd:ReturnAccumulatedDefinition =>
        if (sd.iteration.length == 1) {
          for {
            zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

            intType <- toTargetLanguageType(TypeRep.Int)
            kStart <- explore(sd.iteration.head.inclusiveStart, symbolTable = symbolTable, bottomUp = Some(dp))
            kVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.iteration.head.variable), intType, Some(kStart))
            accVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.accumulationVariable), intType, Some(kStart))

            guardCondition <- explore(sd.iteration.head.guardCondition, symbolTable = symbolTable ++ Map(sd.iteration.head.variable -> kVar, sd.accumulationVariable -> accVar), bottomUp = Some(dp))
            whilestmt <- impParadigm.imperativeCapabilities.whileLoop(guardCondition, for {

              resultExpr <- explore(sd.subproblemExpression, symbolTable = symbolTable ++ Map(sd.iteration.head.variable -> kVar, sd.accumulationVariable -> accVar), bottomUp = Some(dp))
              additive <- arithmetic.arithmeticCapabilities.add(accVar, resultExpr)
              assignResult <- impParadigm.imperativeCapabilities.assignVar(accVar, additive)

              advExpr <- explore(sd.iteration.head.advance, symbolTable = symbolTable ++ Map(sd.iteration.head.variable -> kVar, sd.accumulationVariable -> accVar), bottomUp = Some(dp))
              kadv <- impParadigm.imperativeCapabilities.assignVar(kVar, advExpr)
              _ <- addBlockDefinitions(Seq(assignResult, kadv))

            } yield ())

            retStmt <- impParadigm.imperativeCapabilities.returnStmt(accVar)
          } yield Seq(whilestmt, retStmt)
        } else if (sd.iteration.length == 2) {
          for {
            zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
            one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

            intType <- toTargetLanguageType(TypeRep.Int)
            accVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.accumulationVariable), intType, Some(zero))

            level0_low  <- explore(sd.iteration.head.inclusiveStart, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
            level0_var  <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.iteration.head.variable), intType, Some(level0_low))

            // NOTE that other variables HIGH and LOW might depend on earlier variables, so build up symbol table
            level1_map = Map(sd.iteration.head.variable -> level0_var, sd.accumulationVariable -> accVar)

            // "r"
            level1_low <- explore(sd.iteration.tail.head.inclusiveStart, bottomUp = Some(dp), symbolTable = level1_map, memoize = false)
            level1_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.iteration.tail.head.variable), intType, Some(level1_low))
            level2_map = level1_map ++ Map(sd.iteration.tail.head.variable -> level1_var) // HACK FIX, model.solution.mappers("i") -> expr1)

            level0_condition <- explore(sd.iteration.head.guardCondition, bottomUp = Some(dp), symbolTable = level2_map, memoize = false)
            level1_condition <- explore(sd.iteration.tail.head.guardCondition, bottomUp = Some(dp), symbolTable = level2_map, memoize = false)

            // INNER LOOP
            whileLoop_inner <- impParadigm.imperativeCapabilities.whileLoop(level1_condition, for {
              resultExpr <- explore(sd.subproblemExpression, symbolTable=level2_map, bottomUp=Some(dp))
              additive <- arithmetic.arithmeticCapabilities.add(accVar, resultExpr)
              assignResult <- impParadigm.imperativeCapabilities.assignVar(accVar, additive)

              _ <- addBlockDefinitions(Seq(assignResult))

              ivar_inner_plusone <- arithmetic.arithmeticCapabilities.add(level1_var, one)
              incr_inner <- impParadigm.imperativeCapabilities.assignVar(level1_var, ivar_inner_plusone)

              _ <- addBlockDefinitions(Seq(incr_inner))
            } yield ())

            // OUTER LOOP
            whileLoop_outer <- impParadigm.imperativeCapabilities.whileLoop(level0_condition, for {
              inner_reset <- impParadigm.imperativeCapabilities.assignVar(level1_var, level1_low)

              ivar_outer_plusone <- arithmetic.arithmeticCapabilities.add(level0_var, one)
              incr_outer <- impParadigm.imperativeCapabilities.assignVar(level0_var, ivar_outer_plusone)
              _ <- addBlockDefinitions(Seq(inner_reset, whileLoop_inner, incr_outer))
            } yield ())

            retStmt <- impParadigm.imperativeCapabilities.returnStmt(accVar)
          } yield Seq(whileLoop_outer, retStmt)
        } else {
          ???
        }

      case _ => ???
    }
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

    } yield ()
  }

  def make_bottom_up(model:EnhancedModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      // The approach might force an N-dimensional search space even when the input is only 1-dimensional (like MatrixChainMultiplication)
      // |params| determines the dimensionality of the array dp[][]
      val params = model.mode match {
        case ut:UpperTriangle => ut.params
        case _ => model.solution.order
      }

      for {
        arrayType <- toTargetLanguageType(arTypes(model))

        _ <- forEach(model.input) { bexpr => for {
          tpe <- map_type_in_class(bexpr.argType)
          _ <- addField(names.mangle(bexpr.name), tpe)
        } yield ()
        }

        _ <- addField(dpName, arrayType)   // this becomes "int" if I use arrayType

        constArgs <- forEach(model.input) { bexpr =>
          for {
            tpe <- map_type_in_class(bexpr.argType)
          } yield (names.mangle(bexpr.name), tpe)
        }
        _ <- addConstructor(create_bottom_up_constructor(constArgs))

        // Trying to direct to appropriate place
        _ <- addMethod(computeName, make_bottom_up_compute_method_recursive(model, params))

      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
  }

}
