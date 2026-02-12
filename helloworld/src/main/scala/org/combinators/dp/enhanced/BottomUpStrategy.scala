package org.combinators.dp.enhanced

import org.combinators.dp.Utility
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi._
import org.combinators.ep.generator.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.{Command, NameProvider}
import org.combinators.model.{BooleanType, CharType, Definition, DefinitionStatement, EnhancedModel, ExpressionDefinition, ExpressionStatement, IfThenElseDefinition, IntegerType, MinRangeDefinition, Model}

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
    model.subproblemType match {
      case _:IntegerType =>
        model.input.length match {
          case 1 => TypeRep.Int
          case 2 => TypeRep.Array(TypeRep.Int)
          case 3 => TypeRep.Array(TypeRep.Array(TypeRep.Int))
          case _ =>  ???
        }

      case _:CharType => TypeRep.Char
        model.input.length match {
          case 1 => TypeRep.Char
          case 2 => TypeRep.Array(TypeRep.Char)
          case 3 => TypeRep.Array(TypeRep.Array(TypeRep.Char))
          case _ =>  ???
        }

      case _:BooleanType => TypeRep.Char
        model.input.length match {
          case 1 => TypeRep.Boolean
          case 2 => TypeRep.Array(TypeRep.Boolean)
          case 3 => TypeRep.Array(TypeRep.Array(TypeRep.Boolean))
          case _ =>  ???
        }

      case _ => ???
    }
  }


  /** Needed when working bottom up. */
  private def expand_assign(dp_i:Expression, exp: Expression): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {
      av <- impParadigm.imperativeCapabilities.assignVar(dp_i, exp)
      _ <- addBlockDefinitions(Seq(av))
    } yield None
  }

  // This is hard-coded for a SINGLE bound. We will need another one to deal with two-d problems (and higher)
  def make_bottom_up_compute_method_nest_2(model:EnhancedModel): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

//    val real_cases = model.cases.filter(p => p._1.isDefined) // MUST be at least one.
//    val first_case = real_cases.head
//    val tail_cases = real_cases.tail
//    val elseCase = model.cases.filter(p => p._1.isEmpty) // MUST only be one. Not sure how I would check

    val order = model.solution.order

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
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      // what if two different rectangular?
      max_bound_outer <- max_bound_in_method(model.input.head)
      mboplus1 <- arithmetic.arithmeticCapabilities.add(max_bound_outer, one)
      max_bound_inner <- if (model.input.length > 1) {
        max_bound_in_method(model.input.tail.head)
      } else {
        max_bound_in_method(model.input.head)    // HACK HACK HACK
      }
      mbiplus1 <- arithmetic.arithmeticCapabilities.add(max_bound_inner, one)

      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)

      outer_low <- explore(model.solution.parameters(order(0))._2.low, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      ivar_outer <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(0)), intType, Some(outer_low))
      inner_low <- explore(model.solution.parameters(order(1))._2.low, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      ivar_inner <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(1)), intType, Some(inner_low))

      dp_o <- array.arrayCapabilities.get(dp, ivar_outer)
      dp_o_i <- array.arrayCapabilities.get(dp_o, ivar_inner)
      i_map = Map(order(0) -> ivar_outer)
      j_map = Map(order(1) -> ivar_inner)
      dp_i_j <- array.arrayCapabilities.get(dp_o, ivar_inner)     // FIX ME

      // just wanted to test out (dead code) how maps can be folded and used. This is a template for building
      // up a symbol table when nesting computations.
      ij_map = i_map ++ j_map
      other_data = Seq(i_map, j_map)
      total_map = other_data.foldLeft(Map.empty[String, Expression]) { (acc, a_map) => acc ++ a_map }
      oi_map = Map(order(0) -> ivar_outer, order(1) -> ivar_inner)

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(mboplus1,mbiplus1), None)
      //inner <- explore(first_case._1.get, bottomUp = Some(dp), symbolTable = oi_map)   // get from Model
      inner <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -66)
//      all_rest <- forEach(tail_cases) { next_case =>
//        for {
//          next_cond <- explore(next_case._1.get, memoize = false, bottomUp = Some(dp), symbolTable = oi_map)
//          next_exp <- explore(next_case._2, memoize = false,bottomUp = Some(dp), symbolTable = oi_map)
//        } yield (next_cond, expand_assign(dp_o_i, next_exp))
//      }

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))
      in_range <- arithmetic.arithmeticCapabilities.le(ivar_inner, max_bound_inner)

      out_range <- arithmetic.arithmeticCapabilities.le(ivar_outer, max_bound_outer)

      whileLoop_inner <- impParadigm.imperativeCapabilities.whileLoop(in_range, for {

        av <- generate(dp, dp_o_i, model.definition, symbolTable = oi_map)
        _ <- addBlockDefinitions(av)

        ivar_inner_plusone <- arithmetic.arithmeticCapabilities.add(ivar_inner, one)
        incr_inner <- impParadigm.imperativeCapabilities.assignVar(ivar_inner, ivar_inner_plusone)

        _ <- addBlockDefinitions(Seq(incr_inner))
      } yield ())

      whileLoop_outer <- impParadigm.imperativeCapabilities.whileLoop(out_range, for {
        inner_reset <- impParadigm.imperativeCapabilities.assignVar(ivar_inner, inner_low)

        ivar_outer_plusone <- arithmetic.arithmeticCapabilities.add(ivar_outer, one)
        incr_outer <- impParadigm.imperativeCapabilities.assignVar(ivar_outer, ivar_outer_plusone)
        _ <- addBlockDefinitions(Seq(inner_reset, whileLoop_inner, incr_outer))
        } yield ())

      _ <- addBlockDefinitions(Seq(whileLoop_outer))

      ij = Seq(ivar_outer, ivar_inner)

      // return last element dp[n] because dp is 1 larger in size than n
      dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      maxboundo <- explore(model.solution.parameters(order(0))._1, bottomUp = Some(dp), symbolTable = oi_map)
      maxboundi <- explore(model.solution.parameters(order(1))._1, bottomUp = Some(dp), symbolTable = oi_map)
      dpo <- array.arrayCapabilities.get(dpexp, maxboundo)
      dpi <- array.arrayCapabilities.get(dpo, maxboundi)
      retstmt <- Command.lift(dpi)
    } yield Some(retstmt)
  }

  def exploreExpr(dpij:Expression, defs:DefinitionStatement, symbolTable: Map[String,Expression]) : Generator[paradigm.MethodBodyContext, Expression] = {
    defs match {
      case es:ExpressionStatement => for {
        e <- explore(es.expr, memoize = false, symbolTable = symbolTable, bottomUp=Some(dpij))
      } yield e

      case _ => ???
    }
  }

  def generate (dp:Expression, dpij:Expression, defn:Definition, symbolTable: Map[String,Expression]) : Generator[paradigm.MethodBodyContext, Seq[Statement]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import AnyParadigm.syntax._

    defn match {
      case ed:ExpressionDefinition => for {
        expr <- explore(ed.expr, symbolTable = symbolTable, bottomUp=Some(dp))
        assigned <- impParadigm.imperativeCapabilities.assignVar(dpij, expr)
      } yield Seq(assigned)

      case ite:IfThenElseDefinition => for {
        inner <- explore(ite.condition, symbolTable = symbolTable, bottomUp=Some(dp))
        ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
          // condition of first if
          inner
          ,
          // statements for that first if
          for {
            expr <- exploreExpr(dpij, ite.result, symbolTable = symbolTable)
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

      case ds:MinRangeDefinition => for {
        one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, scala.Int.MaxValue)
        intType <- toTargetLanguageType(TypeRep.Int)   // hack
        minVarName = names.mangle("min")
        minVar <- impParadigm.imperativeCapabilities.declareVar(minVarName, intType, Some(one))   //
        kStart <- explore(ds.inclusiveStart, symbolTable = symbolTable, bottomUp=Some(dp))
        kVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(ds.variable.variable), intType, Some(kStart))   //

        resultVarName = names.mangle("result")
        resultVar <- impParadigm.imperativeCapabilities.declareVar(resultVarName, intType, None)
        addedSymbolTable = symbolTable + ("min" -> minVar) + ("k" -> kVar) + ("result" -> resultVar)

        minCond <- arithmetic.arithmeticCapabilities.lt(resultVar, minVar)
        guardCondition <- explore(ds.guardContinue, symbolTable = addedSymbolTable, bottomUp=Some(dp))
        whilestmt <- impParadigm.imperativeCapabilities.whileLoop(guardCondition, for {
          neg99 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -99)

          resultExpr <- explore(ds.subproblemExpression, symbolTable = addedSymbolTable, bottomUp=Some(dp))
          assignResult <- impParadigm.imperativeCapabilities.assignVar(resultVar, resultExpr)

          // record minimum
          update <- impParadigm.imperativeCapabilities.ifThenElse(minCond, for {
            updateResult <- impParadigm.imperativeCapabilities.assignVar(minVar, resultVar)
            _ <- addBlockDefinitions(Seq(updateResult))
            // here is where one could store deecisions
          } yield (), Seq.empty, None)


          advExpr <- explore(ds.advance, symbolTable=addedSymbolTable, bottomUp=Some(dp))
          kadv <- impParadigm.imperativeCapabilities.assignVar(kVar, advExpr)
          _ <- addBlockDefinitions(Seq(assignResult, update, kadv))
        } yield ())

        assigned <- impParadigm.imperativeCapabilities.assignVar(dpij, minVar)
      } yield Seq(whilestmt, assigned)

      case _ => ???
    }
  }


  // This is hard-coded for a SINGLE bound. We will need another one to deal with two-d problems (and higher)
  def make_bottom_up_compute_method(model:EnhancedModel): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

//    val real_cases = model.cases.filter(p => p._1.isDefined) // MUST be at least one.
//    val first_case = real_cases.head
//    val tail_cases = real_cases.tail
//    val elseCase = model.cases.filter(p => p._1.isEmpty) // MUST only be one. Not sure how I would check

    val order = model.solution.order
    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      theType <- return_type_based_on_model(model)
      _ <- setReturnType(theType)

      // ONLY ONE HERE
      arrayType <- toTargetLanguageType(arTypes(model))

      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
      // seems to be different from Integer which occurs in MethodBodyContext
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      intType <- toTargetLanguageType(TypeRep.Int)
      var_expr <- explore(model.solution.parameters(order(0))._2.low, bottomUp = Some(dp), symbolTable = Map.empty, memoize = false)
      var_var <- impParadigm.imperativeCapabilities.declareVar(names.mangle(order(0)), intType, Some(var_expr))
      dp_i <- array.arrayCapabilities.get(dp, var_var)

      // allocate storage for MAX+1
      max_bound <- max_bound_in_method(model.input.head)
      mbplus1 <- arithmetic.arithmeticCapabilities.add(max_bound, one)
      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(mbplus1), None)

//      inner <- explore(first_case._1.get, bottomUp = Some(dp), symbolTable = Map(model.input.head.itArgName -> ivar))  // get from Model
//
//      all_rest <- forEach(tail_cases) { next_case =>
//        for {
//          next_cond <- explore(next_case._1.get, memoize = false, bottomUp = Some(dp), symbolTable = Map("i" -> ivar))
//          next_exp <- explore(next_case._2, memoize = false, bottomUp = Some(dp), symbolTable = Map("i" -> ivar))
//        } yield (next_cond, expand_assign(dp_i, next_exp))
//      }
      all_rest <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -33)
      inner <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -44)

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))
      in_range <- explore(model.solution.parameters(order(0))._2.in_range, bottomUp = Some(dp), symbolTable = Map(order(0) -> var_var), memoize = false)

      whileLoop <- impParadigm.imperativeCapabilities.whileLoop(in_range, for {
        ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
          // condition of first if
          inner
          ,
          // statements for that first if
          for {
            //resexp <- explore(first_case._2, memoize = false, bottomUp = Some(dp), symbolTable = Map("i" -> ivar))
            resexp <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -11)
            av <- impParadigm.imperativeCapabilities.assignVar(dp_i, resexp)
            _ <- addBlockDefinitions(Seq(av))
          } yield None
          ,
          // collection of (condition, block) for all of the remaining cases
          Seq.empty
          ,
          // terminating 'else' takes the elseCase and adds it last
          Some(for {
            //result_exp <- explore(elseCase.head._2, memoize = false, bottomUp = Some(dp), symbolTable = Map("i" -> ivar))
            result_exp <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -22)
            av <- impParadigm.imperativeCapabilities.assignVar(dp_i, result_exp)
            _ <- addBlockDefinitions(Seq(av))
          } yield ())
        )

        ivarplusone <- arithmetic.arithmeticCapabilities.add(var_var, one)
        incr <- impParadigm.imperativeCapabilities.assignVar(var_var, ivarplusone)

        _ <- addBlockDefinitions(Seq(ifstmt, incr))
      } yield ())

      _ <- addBlockDefinitions(Seq(whileLoop))

      // return last element dp[n] because dp is 1 larger in size than n
      dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      dpn <- array.arrayCapabilities.get(dpexp, max_bound)
      retstmt <- Command.lift(dpn)
    } yield Some(retstmt)
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

  def make_bottom_up_xyz(model:EnhancedModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

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

        _ <- if (model.input.length == 1) {
          addMethod(computeName, make_bottom_up_compute_method(model))
        } else {
          addMethod(computeName, make_bottom_up_compute_method_nest_2(model) )
        }
      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
  }

  def make_bottom_up(model:EnhancedModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

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

        _ <- if (model.solution.order.length == 1) {
          addMethod(computeName, make_bottom_up_compute_method(model))
        } else {
          addMethod(computeName, make_bottom_up_compute_method_nest_2(model) )
        }
      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
  }



}
