package org.combinators.dp.enhanced

import org.combinators.dp.Utility
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi._
import org.combinators.ep.generator.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.{Command, NameProvider}
import org.combinators.models.{BooleanType, CharType, Definition, DefinitionStatement, EnhancedModel, ExpressionDefinition, ExpressionStatement, IfThenElseDefinition, IntegerType, MaxRangeDefinition, MinRangeDefinition, ReturnAccumulatedDefinition, ReturnExpressionDefinition, SumDefinition, UpperTriangle}

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

  // Definition of the name of the helper method
  val computeName: Name
  val keyName: Name

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

  def make_bottom_up_compute_method_nest_3(model:EnhancedModel, order:Seq[String]): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      theType <- return_type_based_on_model(model)
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(theType)
      // ONLY ONE HERE
      arrayType <- toTargetLanguageType(arTypes(model))

      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
      // seems to be different from Integer which occurs in MethodBodyContext
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

      // level 0 is the outermost variable while level 2 is the innermost
      level0_low <- explore(model.find(order(0)).low, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      level0_high <- explore(model.find(order(0)).high, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      level0_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(0)), intType, Some(level0_low))
      level1_map = Map(order(0) -> level0_var)

      level1_low <- explore(model.find(order(1)).low, bottomUp = Some(dp), symbolTable = level1_map, memoize = false)
      level1_high <- explore(model.find(order(1)).high, bottomUp = Some(dp), symbolTable = level1_map, memoize = false)
      level1_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(1)), intType, Some(level1_low))
      level2_map = level1_map ++ Map(order(1) -> level1_var)

      level2_low <- explore(model.find(order(2)).low, bottomUp = Some(dp), symbolTable = level2_map, memoize = false)
      level2_high <- explore(model.find(order(2)).high, bottomUp = Some(dp), symbolTable = level2_map, memoize = false)
      level2_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(2)), intType, Some(level2_low))
      level3_map = level2_map ++ Map(order(2) -> level2_var)

      expr1 <- explore(model.find_map(order(0)), bottomUp = Some(dp), symbolTable = level3_map)
      expr2 <- explore(model.find_map(order(1)), bottomUp = Some(dp), symbolTable = level3_map)
      expr3 <- explore(model.find_map(order(2)), bottomUp = Some(dp), symbolTable = level3_map)

      dp_0 <- array.arrayCapabilities.get(dp, Seq(expr1))
      dp_0_1 <- array.arrayCapabilities.get(dp_0, Seq(expr2))
      dp_0_1_2 <- array.arrayCapabilities.get(dp_0_1, Seq(expr3))

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(level0_high, level1_high, level2_high), None)

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))

      level0_condition <- explore(model.find(order(0)).in_range, bottomUp = Some(dp), symbolTable = level3_map, memoize = false)
      level1_condition <- explore(model.find(order(1)).in_range, bottomUp = Some(dp), symbolTable = level3_map, memoize = false)
      level2_condition <- explore(model.find(order(2)).in_range, bottomUp = Some(dp), symbolTable = level3_map, memoize = false)

      // INNERMOST loop
      level2_whileLoop <- impParadigm.imperativeCapabilities.whileLoop(level2_condition, for {
        av <- generate(dp, dp_0_1_2, model.definition, symbolTable = level3_map)
        _ <- addBlockDefinitions(av)

        level2_var_plusone <- arithmetic.arithmeticCapabilities.add(level2_var, one)
        incr_inner <- impParadigm.imperativeCapabilities.assignVar(level2_var, level2_var_plusone)

        _ <- addBlockDefinitions(Seq(incr_inner))
      } yield ())

      // NEXT ONE OUTSIDE of INNERMOST
      level1_whileLoop <- impParadigm.imperativeCapabilities.whileLoop(level1_condition, for {
        level2_reset <- impParadigm.imperativeCapabilities.assignVar(level2_var, level2_low)

        level1_var_plusone <- arithmetic.arithmeticCapabilities.add(level1_var, one)
        incr_outer <- impParadigm.imperativeCapabilities.assignVar(level1_var, level1_var_plusone)
        _ <- addBlockDefinitions(Seq(level2_reset, level2_whileLoop, incr_outer))
      } yield ())

      // OUTSIDE LOOP
      level0_whileLoop <- impParadigm.imperativeCapabilities.whileLoop(level0_condition, for {
        level1_reset <- impParadigm.imperativeCapabilities.assignVar(level1_var, level1_low)

        level0_var_plusone <- arithmetic.arithmeticCapabilities.add(level0_var, one)
        incr_outer <- impParadigm.imperativeCapabilities.assignVar(level0_var, level0_var_plusone)
        _ <- addBlockDefinitions(Seq(level1_reset, level1_whileLoop, incr_outer))
      } yield ())

      _ <- addBlockDefinitions(Seq(level0_whileLoop))

      // return last element dp[n] because dp is 1 larger in size than n
      dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

      //expr <- explore(model.answer, memoize = false, bottomUp = Some(dpexp), symbolTable = level3_map)  // At this point, there should be no symbols
      av <- generate(dp, dp_0_1_2, model.answer, symbolTable = level3_map)
      _ <- addBlockDefinitions(av)

    } yield None
  }

//  def make_bottom_up_compute_method_arbitrary_nesting (model:EnhancedModel): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
//    import paradigm.methodBodyCapabilities._
//
//    val order = model.solution.order   // typically, "i", "j", "k"
//    val emptyMap:Map[String,Expression] = Map.empty
//    val emptySeq:Seq[Expression] = Seq.empty
//    for {
//      self <- ooParadigm.methodBodyCapabilities.selfReference()
//      theType <- return_type_based_on_model(model)
//      intType <- toTargetLanguageType(TypeRep.Int)
//      _ <- setReturnType(theType)
//      // ONLY ONE HERE
//      arrayType <- toTargetLanguageType(arTypes(model))
//
//      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
//      // seems to be different from Integer which occurs in MethodBodyContext
//      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
//
//      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
//
//      loop_frames <- forEach(order) { variable => for {
//          low <- explore(model.solution.helpers(variable).low, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
//          high <- explore(model.solution.helpers(variable).high, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
//          declared <- impParadigm.imperativeCapabilities.declareVar(names.mangle(variable), intType, Some(low))
//        } yield (variable, low, high, declared)
//      }
//
//      dp_access = loop_frames.foldLeft(dp)((acc, frame) => {
//        for {
//          wrap <- array.arrayCapabilities.get(acc, frame._4)
//        } yield wrap
//      })
//
//      oi_map <- for {
//        n76 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -76)   // WHY WHY WHY
//        result = loop_frames.foldLeft(emptyMap)((acc, frame) => acc + (frame._1 -> frame._4))
//      } yield result
//
//      highs <- for {
//        n76 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -76)   // WHY WHY WHY
//        result = loop_frames.foldLeft(emptySeq)((acc, frame) => acc :+ frame._3)
//      } yield result
//
//      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, highs, None)
//
//      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
//      _ <- addBlockDefinitions(Seq(assign_stmt))
//
//      conditions <- forEach(order) { variable =>
//        for {
//          condition <- explore(model.solution.helpers(variable).in_range, bottomUp = Some(dp), symbolTable = oi_map, memoize = false)
//        } yield condition
//      }
//
//      nugget <- generate(dp, dp_access, model.definition, symbolTable = oi_map)
//      while_loops = (loop_frames zip conditions).foldRight(nugget)((pair, acc) => for {
//        loop <- impParadigm.imperativeCapabilities.whileLoop(pair._2, for {
//          av <- generate(dp, dp_access, model.definition, symbolTable = oi_map)
//          _ <- addBlockDefinitions(av)
//
//          var_plusone <- arithmetic.arithmeticCapabilities.add(pair._1._4, one)
//          incr_inner <- impParadigm.imperativeCapabilities.assignVar(pair._1._4, var_plusone)
//
//          _ <- addBlockDefinitions(acc :+ incr_inner)
//        } yield ())
//      } yield loop)
//      _ <- addBlockDefinitions(while_loops)
//
////      // INNERMOST loop
////      level2_whileLoop <- impParadigm.imperativeCapabilities.whileLoop(level2_condition, for {
////        av <- generate(dp, dp_0_1_2, model.definition, symbolTable = oi_map)
////        _ <- addBlockDefinitions(av)
////
////        level2_var_plusone <- arithmetic.arithmeticCapabilities.add(level2_var, one)
////        incr_inner <- impParadigm.imperativeCapabilities.assignVar(level2_var, level2_var_plusone)
////
////        _ <- addBlockDefinitions(Seq(incr_inner))
////      } yield ())
////
////      // NEXT ONE OUTSIDE of INNERMOST
////      level1_whileLoop <- impParadigm.imperativeCapabilities.whileLoop(level1_condition, for {
////        level2_reset <- impParadigm.imperativeCapabilities.assignVar(level2_var, level2_low)
////
////        level1_var_plusone <- arithmetic.arithmeticCapabilities.add(level1_var, one)
////        incr_outer <- impParadigm.imperativeCapabilities.assignVar(level1_var, level1_var_plusone)
////        _ <- addBlockDefinitions(Seq(level2_reset, level2_whileLoop, incr_outer))
////      } yield ())
////
////      // OUTSIDE LOOP
////      level0_whileLoop <- impParadigm.imperativeCapabilities.whileLoop(level0_condition, for {
////        level1_reset <- impParadigm.imperativeCapabilities.assignVar(level1_var, level1_low)
////
////        level0_var_plusone <- arithmetic.arithmeticCapabilities.add(level0_var, one)
////        incr_outer <- impParadigm.imperativeCapabilities.assignVar(level0_var, level0_var_plusone)
////        _ <- addBlockDefinitions(Seq(level1_reset, level1_whileLoop, incr_outer))
////      } yield ())
//
//     // _ <- addBlockDefinitions(Seq(level0_whileLoop))
//
//      // return last element dp[n] because dp is 1 larger in size than n
//      dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
//      maxbound0 <- explore(model.solution.parameters(order(0)), bottomUp = Some(dp), symbolTable = oi_map)
//      maxbound1 <- explore(model.solution.parameters(order(1)), bottomUp = Some(dp), symbolTable = oi_map)
//      maxbound2 <- explore(model.solution.parameters(order(2)), bottomUp = Some(dp), symbolTable = oi_map)
//      dp0 <- array.arrayCapabilities.get(dpexp, maxbound0)
//      dp1 <- array.arrayCapabilities.get(dp0, maxbound1)
//      dp2 <- array.arrayCapabilities.get(dp1, maxbound2)
//      retstmt <- Command.lift(dp2)
//    } yield Some(retstmt)
//  }

  def report(str:String) : Generator[paradigm.MethodBodyContext, Unit] = {
    println(str)
    for  {
      ne77 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -77)
    } yield ()
  }

  // This is hard-coded for a SINGLE bound. We will need another one to deal with two-d problems (and higher)
  def make_bottom_up_compute_method_nest_2(model:EnhancedModel, order:Seq[String]): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      theType <- return_type_based_on_model(model)
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(theType)

      // ONLY ONE HERE
      arrayType <- toTargetLanguageType(arTypes(model))

      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
      // seems to be different from Integer which occurs in MethodBodyContext
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

      // "c"
      level0_low  <- explore(model.find(order(0)).low, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      level0_high <- explore(model.find(order(0)).high, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      level0_var  <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(0)), intType, Some(level0_low))

      // NOTE that other variables HIGH and LOW might depend on earlier variables, so build up symbol table
      level1_map = Map(order(0) -> level0_var)

      // "r"
      level1_low <- explore(model.find(order(1)).low, bottomUp = Some(dp), symbolTable = level1_map, memoize = false)
      level1_high <- explore(model.find(order(1)).high, bottomUp = Some(dp), symbolTable = level1_map, memoize = false)

      level1_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(1)), intType, Some(level1_low))
      level2_map = level1_map ++ Map(order(1) -> level1_var) // HACK FIX, model.solution.mappers("i") -> expr1)

      expr1 <- explore(model.find_map(order(0)), bottomUp = Some(dp), symbolTable = level2_map)
      expr2 <- explore(model.find_map(order(1)), bottomUp = Some(dp), symbolTable = level2_map) // needed?? ++ Map(order(0) -> expr1))   // HACK)

      level0_condition <- explore(model.find(order(0)).in_range, bottomUp = Some(dp), symbolTable = level2_map, memoize = false)
      level1_condition <- explore(model.find(order(1)).in_range, bottomUp = Some(dp), symbolTable = level2_map, memoize = false)

      dp_o <- array.arrayCapabilities.get(dp, Seq(expr1))        // needs to be [i] NOT level0_var -- can be overridden with mapper
      dp_o_i <- array.arrayCapabilities.get(dp_o, Seq(expr2))    // needs to be [j] NOT level1_var)

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(level0_high, level1_high), None)

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))

      // INNER LOOP
      whileLoop_inner <- impParadigm.imperativeCapabilities.whileLoop(level1_condition, for {
        av <- generate(dp, dp_o_i, model.definition, symbolTable = level2_map)
        _ <- addBlockDefinitions(av)

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

      _ <- addBlockDefinitions(Seq(whileLoop_outer))

      // return last element dp[n] because dp is 1 larger in size than n
      dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

      // expr <- explore(model.answer, memoize = false, bottomUp = Some(dpexp), symbolTable = level2_map)  // At this point, there should be no symbols
      av <- generate(dp, dp_o_i, model.answer, symbolTable=level2_map)
      _ <- addBlockDefinitions(av)

    } yield None
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
          neg99 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -99)

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
        zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
        intType <- toTargetLanguageType(TypeRep.Int) // perhaps acceptable to consider 'min' will be an integer

        intType <- toTargetLanguageType(TypeRep.Int) // perhaps acceptable to consider 'min' will be an integer
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
            kStart <- explore(sd.iteration.head._2, symbolTable = symbolTable, bottomUp = Some(dp))
            kVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.iteration.head._1), intType, Some(kStart))
            accVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.accumulationVariable), intType, Some(kStart))

            guardCondition <- explore(sd.iteration.head._3, symbolTable = symbolTable ++ Map(sd.iteration.head._1 -> kVar, sd.accumulationVariable -> accVar), bottomUp = Some(dp))
            whilestmt <- impParadigm.imperativeCapabilities.whileLoop(guardCondition, for {

              resultExpr <- explore(sd.subproblemExpression, symbolTable = symbolTable ++ Map(sd.iteration.head._1 -> kVar, sd.accumulationVariable -> accVar), bottomUp = Some(dp))
              additive <- arithmetic.arithmeticCapabilities.add(accVar, resultExpr)
              assignResult <- impParadigm.imperativeCapabilities.assignVar(accVar, additive)

              advExpr <- explore(sd.iteration.head._4, symbolTable = symbolTable ++ Map(sd.iteration.head._1 -> kVar, sd.accumulationVariable -> accVar), bottomUp = Some(dp))
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

            level0_low  <- explore(sd.iteration.head._2, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
            level0_var  <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.iteration.head._1), intType, Some(level0_low))

            // NOTE that other variables HIGH and LOW might depend on earlier variables, so build up symbol table
            level1_map = Map(sd.iteration.head._1 -> level0_var, sd.accumulationVariable -> accVar)

            // "r"
            level1_low <- explore(sd.iteration.tail.head._2, bottomUp = Some(dp), symbolTable = level1_map, memoize = false)
            level1_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(sd.iteration.tail.head._1), intType, Some(level1_low))
            level2_map = level1_map ++ Map(sd.iteration.tail.head._1 -> level1_var) // HACK FIX, model.solution.mappers("i") -> expr1)

            level0_condition <- explore(sd.iteration.head._3, bottomUp = Some(dp), symbolTable = level2_map, memoize = false)
            level1_condition <- explore(sd.iteration.tail.head._3, bottomUp = Some(dp), symbolTable = level2_map, memoize = false)

            dp_o <- array.arrayCapabilities.get(dp, Seq(level0_var))        // needs to be [i] NOT level0_var -- can be overridden with mapper
            dp_o_i <- array.arrayCapabilities.get(dp_o, Seq(level1_var))    // needs to be [j] NOT level1_var)


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
          // cannot handle 3-dimensions or more
          ???
        }

      case _ => ???
    }
  }

  // This is hard-coded for a SINGLE bound. We will need another one to deal with two-d problems (and higher)
  def make_bottom_up_compute_method(model:EnhancedModel, order:Seq[String]): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      theType <- return_type_based_on_model(model)
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(theType)

      // ONLY ONE HERE
      arrayType <- toTargetLanguageType(arTypes(model))

      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
      // seems to be different from Integer which occurs in MethodBodyContext
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

      // level 0 is the outermost variable while level 2 is the innermost
      level0_low <- explore(model.find(order(0)).low, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      level0_high <- explore(model.find(order(0)).high, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      level0_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(0)), intType, Some(level0_low))
      oi_map = Map(order(0) -> level0_var)

      expr1 <- explore(model.find_map(order(0)), bottomUp = Some(dp), symbolTable = oi_map)

      dp_0 <- array.arrayCapabilities.get(dp, Seq(expr1))

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(level0_high), None)

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))

      level0_condition <- explore(model.find(order(0)).in_range, bottomUp = Some(dp), symbolTable = oi_map, memoize = false)

      whileLoop <- impParadigm.imperativeCapabilities.whileLoop(level0_condition, for {
          av <- generate(dp, dp_0, model.definition, symbolTable = oi_map)
          _ <- addBlockDefinitions(av)

          level0_var_plusone <- arithmetic.arithmeticCapabilities.add(level0_var, one)
          incr_outer <- impParadigm.imperativeCapabilities.assignVar(level0_var, level0_var_plusone)

        _ <- addBlockDefinitions(Seq(incr_outer))
      } yield ())

      _ <- addBlockDefinitions(Seq(whileLoop))

      // return last element dp[n] because dp is 1 larger in size than n
      //dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      //expr <- explore(model.answer, memoize = false, bottomUp = Some(dpexp), symbolTable = oi_map)  // At this point, there should be no symbols

      av <- generate(dp, dp_0, model.answer, symbolTable=oi_map)
      _ <- addBlockDefinitions(av)

    } yield None
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

  def make_bottom_up(model:EnhancedModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      // The approach might force an N-dimensional search space even when the input is only 1-dimensional (like MatrixChainMultiplication)
      // |params| determins the dimensionality of the array dp[][]
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
        _ <- if (params.length == 3) {
          addMethod(computeName, make_bottom_up_compute_method_nest_3(model, params))
        } else if (params.length == 2) {
          addMethod(computeName, make_bottom_up_compute_method_nest_2(model, params) )
        } else if (params.length == 1) {
          addMethod(computeName, make_bottom_up_compute_method(model, params))
        } else {
          ???
        }
      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
  }

}
