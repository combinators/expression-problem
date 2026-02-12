package org.combinators.dp

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.{Command, NameProvider}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Equality, Strings}
import org.combinators.model.{AdditionExpression, ArgumentType, EqualExpression, InputExpression, IteratorExpression, LiteralInt, Model, StringLengthExpression, SubproblemExpression, SubtractionExpression}

/**
 * Concepts necessary to realize top-down solutions
 */

trait BottomUpStrategy extends Utility {
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

  lazy val iName      = names.mangle("i")
  lazy val nName      = names.mangle("n")
  lazy val dpName     = names.mangle("dp")

  // will need to be expanded to depth-10 or something
  lazy val arTypes = Seq(TypeRep.Int,
                         TypeRep.Array(TypeRep.Int),
                         TypeRep.Array(TypeRep.Array(TypeRep.Int)),
                         TypeRep.Array(TypeRep.Array(TypeRep.Array(TypeRep.Int))),
                         TypeRep.Array(TypeRep.Array(TypeRep.Array(TypeRep.Array(TypeRep.Int)))),
  )

  /** Needed when working bottom up. */
  private def expand_assign(dp_i:Expression, exp: Expression): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    for {
      av <- impParadigm.imperativeCapabilities.assignVar(dp_i, exp)
      _ <- addBlockDefinitions(Seq(av))
    } yield None
  }

  def make_bottom_up_compute_method_nest_3(model: Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    val real_cases = model.cases.filter(p => p._1.isDefined) // MUST be at least one.
    val first_case = real_cases.head
    val tail_cases = real_cases.tail
    val elseCase = model.cases.filter(p => p._1.isEmpty) // MUST only be one.

    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)

      arrayType <- toTargetLanguageType(arTypes(model.bounds.length))

      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      // Get max bounds for all three dimensions
      max_bound_outer <- max_bound_in_method(model.bounds.head)
      mboplus1 <- arithmetic.arithmeticCapabilities.add(max_bound_outer, one)

      max_bound_middle <- max_bound_in_method(model.bounds.tail.head)
      mbmplus1 <- arithmetic.arithmeticCapabilities.add(max_bound_middle, one)

      max_bound_inner <- max_bound_in_method(model.bounds.tail.tail.head)
      mbiplus1 <- arithmetic.arithmeticCapabilities.add(max_bound_inner, one)

      // Declare iteration variables for all three dimensions
      ivar_outer <- impParadigm.imperativeCapabilities.declareVar(names.mangle(model.bounds.head.itArgName), intType, Some(zero))
      ivar_middle <- impParadigm.imperativeCapabilities.declareVar(names.mangle(model.bounds.tail.head.itArgName), intType, Some(zero))
      ivar_inner <- impParadigm.imperativeCapabilities.declareVar(names.mangle(model.bounds.tail.tail.head.itArgName), intType, Some(zero))

      // Access dp array at all three levels
      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      dp_o <- array.arrayCapabilities.get(dp, ivar_outer)
      dp_o_m <- array.arrayCapabilities.get(dp_o, ivar_middle)
      dp_o_m_i <- array.arrayCapabilities.get(dp_o_m, ivar_inner)

      // Build symbol table with all three iteration variables
      symbol_table = Map(
        model.bounds.head.itArgName -> ivar_outer,
        model.bounds.tail.head.itArgName -> ivar_middle,
        model.bounds.tail.tail.head.itArgName -> ivar_inner
      )

      // Instantiate 3D array
      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(mboplus1, mbmplus1, mbiplus1), None)

      inner <- explore(first_case._1.get, bottomUp = Some(dp), symbolTable = symbol_table)

      all_rest <- forEach(tail_cases) { next_case =>
        for {
          next_cond <- explore(next_case._1.get, memoize = false, bottomUp = Some(dp), symbolTable = symbol_table)
          next_exp <- explore(next_case._2, memoize = false, bottomUp = Some(dp), symbolTable = symbol_table)
        } yield (next_cond, expand_assign(dp_o_m_i, next_exp))
      }

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar(dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))

      // Range conditions for all three dimensions
      in_range <- arithmetic.arithmeticCapabilities.le(ivar_inner, max_bound_inner)
      middle_range <- arithmetic.arithmeticCapabilities.le(ivar_middle, max_bound_middle)
      out_range <- arithmetic.arithmeticCapabilities.le(ivar_outer, max_bound_outer)

      // Innermost while loop (inner dimension)
      whileLoop_inner <- impParadigm.imperativeCapabilities.whileLoop(in_range, for {
        ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
          inner,
          for {
            resexp <- explore(first_case._2, memoize = false, bottomUp = Some(dp), symbolTable = symbol_table)
            av <- impParadigm.imperativeCapabilities.assignVar(dp_o_m_i, resexp)
            _ <- addBlockDefinitions(Seq(av))
          } yield None,
          all_rest,
          Some(for {
            result_exp <- explore(elseCase.head._2, memoize = false, bottomUp = Some(dp), symbolTable = symbol_table)
            av <- impParadigm.imperativeCapabilities.assignVar(dp_o_m_i, result_exp)
            _ <- addBlockDefinitions(Seq(av))
          } yield ())
        )

        ivar_inner_plusone <- arithmetic.arithmeticCapabilities.add(ivar_inner, one)
        incr_inner <- impParadigm.imperativeCapabilities.assignVar(ivar_inner, ivar_inner_plusone)

        _ <- addBlockDefinitions(Seq(ifstmt, incr_inner))
      } yield ())

      // Middle while loop
      whileLoop_middle <- impParadigm.imperativeCapabilities.whileLoop(middle_range, for {
        inner_reset <- impParadigm.imperativeCapabilities.assignVar(ivar_inner, zero)

        ivar_middle_plusone <- arithmetic.arithmeticCapabilities.add(ivar_middle, one)
        incr_middle <- impParadigm.imperativeCapabilities.assignVar(ivar_middle, ivar_middle_plusone)

        _ <- addBlockDefinitions(Seq(inner_reset, whileLoop_inner, incr_middle))
      } yield ())

      // Outermost while loop
      whileLoop_outer <- impParadigm.imperativeCapabilities.whileLoop(out_range, for {
        middle_reset <- impParadigm.imperativeCapabilities.assignVar(ivar_middle, zero)
        inner_reset <- impParadigm.imperativeCapabilities.assignVar(ivar_inner, zero)

        ivar_outer_plusone <- arithmetic.arithmeticCapabilities.add(ivar_outer, one)
        incr_outer <- impParadigm.imperativeCapabilities.assignVar(ivar_outer, ivar_outer_plusone)

        _ <- addBlockDefinitions(Seq(middle_reset, inner_reset, whileLoop_middle, incr_outer))
      } yield ())

      _ <- addBlockDefinitions(Seq(whileLoop_outer))

      // Return last element dp[max_bound_outer][max_bound_middle][max_bound_inner]
      dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      dpo <- array.arrayCapabilities.get(dpexp, max_bound_outer)
      dpm <- array.arrayCapabilities.get(dpo, max_bound_middle)
      dpi <- array.arrayCapabilities.get(dpm, max_bound_inner)
      retstmt <- Command.lift(dpi)
    } yield Some(retstmt)
  }

  // This is hard-coded for TWO bounds.
  def make_bottom_up_compute_method_nest_2(model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    val real_cases = model.cases.filter(p => p._1.isDefined) // MUST be at least one.
    val first_case = real_cases.head
    val tail_cases = real_cases.tail
    val elseCase = model.cases.filter(p => p._1.isEmpty) // MUST only be one. Not sure how I would check

    for {
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      // ONLY ONE HERE
      arrayType <- toTargetLanguageType(arTypes(model.bounds.length))

      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
      // seems to be different from Integer which occurs in MethodBodyContext
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      max_bound_outer <- max_bound_in_method(model.bounds.head)
      mboplus1 <- arithmetic.arithmeticCapabilities.add(max_bound_outer, one)

      max_bound_inner <- max_bound_in_method(model.bounds.tail.head)
      mbiplus1 <- arithmetic.arithmeticCapabilities.add(max_bound_inner, one)

      ivar_outer <- impParadigm.imperativeCapabilities.declareVar(names.mangle(model.bounds.head.itArgName), intType, Some(zero))
      ivar_inner <- impParadigm.imperativeCapabilities.declareVar(names.mangle(model.bounds.tail.head.itArgName), intType, Some(zero))

      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      dp_o <- array.arrayCapabilities.get(dp, ivar_outer)
      dp_o_i <- array.arrayCapabilities.get(dp_o, ivar_inner)
      i_map = Map(model.bounds.head.itArgName -> ivar_outer)
      j_map = Map(model.bounds.tail.head.itArgName -> ivar_inner)

      // just wanted to test out (dead code) how maps can be folded and used. This is a template for building
      // up a symbol table when nesting computations.
      ij_map = i_map ++ j_map
      other_data = Seq(i_map, j_map)
      total_map = other_data.foldLeft(Map.empty[String, Expression]) { (acc, a_map) => acc ++ a_map }
      oi_map = Map(model.bounds.head.itArgName -> ivar_outer, model.bounds.tail.head.itArgName -> ivar_inner)

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(mboplus1,mbiplus1), None)
      inner <- explore(first_case._1.get, bottomUp = Some(dp), symbolTable = oi_map)   // get from Model

      all_rest <- forEach(tail_cases) { next_case =>
        for {
          next_cond <- explore(next_case._1.get, memoize = false, bottomUp = Some(dp), symbolTable= oi_map)
          next_exp <- explore(next_case._2, memoize = false,  bottomUp = Some(dp), symbolTable= oi_map)
        } yield (next_cond, expand_assign(dp_o_i, next_exp))
      }

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))
      in_range <- arithmetic.arithmeticCapabilities.le(ivar_inner, max_bound_inner)

      out_range <- arithmetic.arithmeticCapabilities.le(ivar_outer, max_bound_outer)

      whileLoop_inner <- impParadigm.imperativeCapabilities.whileLoop(in_range, for {

        ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
          // condition of first if
          inner
          ,
          // statements for that first if
          for {
            resexp <- explore(first_case._2, memoize = false,  bottomUp = Some(dp), symbolTable= oi_map)
            av <- impParadigm.imperativeCapabilities.assignVar(dp_o_i, resexp)
            _ <- addBlockDefinitions(Seq(av))
          } yield None
          ,
          // collection of (condition, block) for all of the remaining cases
          all_rest
          ,
          // terminating 'else' takes the elseCase and adds it last
          Some(for {
            result_exp <- explore(elseCase.head._2, memoize = false,  bottomUp = Some(dp), symbolTable= oi_map)
            av <- impParadigm.imperativeCapabilities.assignVar(dp_o_i, result_exp)
            _ <- addBlockDefinitions(Seq(av))
          } yield ())
        )

        ivar_inner_plusone <- arithmetic.arithmeticCapabilities.add(ivar_inner, one)
        incr_inner <- impParadigm.imperativeCapabilities.assignVar(ivar_inner, ivar_inner_plusone)

        _ <- addBlockDefinitions(Seq(ifstmt, incr_inner))
      } yield ())

      whileLoop_outer <- impParadigm.imperativeCapabilities.whileLoop(out_range, for {
        inner_reset <- impParadigm.imperativeCapabilities.assignVar(ivar_inner, zero)

        ivar_outer_plusone <- arithmetic.arithmeticCapabilities.add(ivar_outer, one)
        incr_outer <- impParadigm.imperativeCapabilities.assignVar(ivar_outer, ivar_outer_plusone)
        _ <- addBlockDefinitions(Seq(inner_reset, whileLoop_inner, incr_outer))
        } yield ())

      _ <- addBlockDefinitions(Seq(whileLoop_outer))

      ij = Seq(ivar_outer, ivar_inner)



      // return last element dp[n] because dp is 1 larger in size than n
      dpexp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      dpo <- array.arrayCapabilities.get(dpexp, max_bound_outer)
      dpi <- array.arrayCapabilities.get(dpo, max_bound_inner)
      retstmt <- Command.lift(dpi)
    } yield Some(retstmt)
  }


  // This is hard-coded for a SINGLE bound. We will need another one to deal with two-d problems (and higher)
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

      // ONLY ONE HERE
      arrayType <- toTargetLanguageType(arTypes(model.bounds.length))

      // cannot seem to do this in Constructor because it insists on using "int" for TypeRep.Int within ConstructorContext which
      // seems to be different from Integer which occurs in MethodBodyContext
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      max_bound <- max_bound_in_method(model.bounds.head)
      mbplus1 <- arithmetic.arithmeticCapabilities.add(max_bound, one)

      ivar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(model.bounds.head.itArgName), intType, Some(zero))

      dp <- ooParadigm.methodBodyCapabilities.getMember(self, dpName)
      dp_i <- array.arrayCapabilities.get(dp, ivar)

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(arrayType, Seq(mbplus1), None)

      inner <- explore(first_case._1.get, bottomUp = Some(dp), symbolTable = Map(model.bounds.head.itArgName -> ivar))  // get from Model

      all_rest <- forEach(tail_cases) { next_case =>
        for {
          next_cond <- explore(next_case._1.get, memoize = false, bottomUp = Some(dp), symbolTable = Map("i" -> ivar))
          next_exp <- explore(next_case._2, memoize = false, bottomUp = Some(dp), symbolTable = Map("i" -> ivar))
        } yield (next_cond, expand_assign(dp_i, next_exp))
      }

      assign_stmt <- impParadigm.imperativeCapabilities.assignVar (dp, instantiated)
      _ <- addBlockDefinitions(Seq(assign_stmt))
      in_range <- arithmetic.arithmeticCapabilities.le(ivar, max_bound)

      whileLoop <- impParadigm.imperativeCapabilities.whileLoop(in_range, for {
        ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(
          // condition of first if
          inner
          ,
          // statements for that first if
          for {
            resexp <- explore(first_case._2, memoize = false, bottomUp = Some(dp), symbolTable = Map("i" -> ivar))
            av <- impParadigm.imperativeCapabilities.assignVar(dp_i, resexp)
            _ <- addBlockDefinitions(Seq(av))
          } yield None
          ,
          // collection of (condition, block) for all of the remaining cases
          all_rest
          ,
          // terminating 'else' takes the elseCase and adds it last
          Some(for {
            result_exp <- explore(elseCase.head._2, memoize = false, bottomUp = Some(dp), symbolTable = Map("i" -> ivar))
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

  def make_bottom_up(model:Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._

      for {
        arrayType <- toTargetLanguageType(arTypes(model.bounds.length))

        _ <- forEach(model.bounds) { bexpr => for {
            tpe <- map_type_in_class(bexpr.argType)
            _ <- addField(names.mangle(bexpr.name), tpe)
          } yield ()
        }

        _ <- addField(dpName, arrayType)   // this becomes "int" if I use arrayType

        constArgs <- forEach(model.bounds) { bexpr =>
          for {
            tpe <- map_type_in_class(bexpr.argType)
          } yield (names.mangle(bexpr.name), tpe)
        }
        _ <- addConstructor(create_bottom_up_constructor(constArgs))

        _ <- if (model.bounds.length == 1) {
          addMethod(computeName, make_bottom_up_compute_method(model))
        } else if (model.bounds.length == 2) {
          addMethod(computeName, make_bottom_up_compute_method_nest_2(model) )
        } else {
          addMethod(computeName, make_bottom_up_compute_method_nest_3(model))
        }
      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
  }

}
