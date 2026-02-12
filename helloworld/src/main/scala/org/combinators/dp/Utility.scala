package org.combinators.dp

import org.combinators.model.{AdditionExpression, AndExpression, ArgExpression, ArgumentType, ArrayElementExpression, ArrayLengthExpression, CharAtExpression, EqualExpression, HelperExpression, InputExpression, IntegerType, IteratorExpression, LessThanExpression, LessThanOrEqualExpression, LiteralBoolean, LiteralInt, LiteralString, MaxExpression, MinExpression, Model, MultiplicationExpression, OrExpression, SelfExpression, StringLengthExpression, StringType, SubproblemExpression, SubtractionExpression, TernaryExpression}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Equality, RealArithmetic, Strings}

// Different approach
trait GenerationOption {
  def name:String
}

case class TopDown(memo:Boolean = false) extends GenerationOption {
  def name:String = if (memo) { "topDownMemo" } else { "topDown" }
}

case class BottomUp() extends GenerationOption {
  def name:String = "bottomUp"
}

trait Utility {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val realArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  // known to ALL approaches (top-down or bottom up)
  lazy val helperName     = names.mangle("helper")
  lazy val computeName    = names.mangle("compute")

  class DPExample[Input, Output, Full_Solution] (val name:String, val example:Input, val solution:Output, val full_solution:Full_Solution) {
  }

  def pair_helper(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {

      //Setting up header
      argType <- toTargetLanguageType(TypeRep.Int)
      argName1 = names.mangle("i")
      argName2 = names.mangle("j")

      _ <- setParameters(Seq((argName1,argType),(argName2,argType)))

      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)

      //Function details
      args <- getArguments()
      i = args.head._3
      j = args.tail.head._3

      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)

      ipj <- arithmetic.arithmeticCapabilities.add(i, j)
      ipjp1 <-  arithmetic.arithmeticCapabilities.add(ipj, one)

      ipjtipjp1 <- arithmetic.arithmeticCapabilities.mult(ipj, ipjp1)

      ipjtipjp1o2 <- arithmetic.arithmeticCapabilities.div(ipjtipjp1, two)

      finalExpression <- arithmetic.arithmeticCapabilities.add(ipjtipjp1o2, i)


    } yield Some(finalExpression)
  }


  def generate_DP_int_array_test[FS](clazz:Name, tests:Seq[DPExample[Seq[Int],Int,FS]]): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._
    import AnyParadigm.syntax._
    for {
      solutionType <- ooParadigm.methodBodyCapabilities.findClass(clazz)
      sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solutionType, Seq.empty)
      computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, names.mangle("compute"))
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))

      assert_statements <- forEach(tests) { example =>
        for {
          expr <- create_int_array(example.example)
          variable <- impParadigm.imperativeCapabilities.declareVar(names.mangle(example.name), arrayType, Some(expr))
          invoke <- apply(computeMethod, Seq(variable))
          solution <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, example.solution)
          assert_stmt <- asserts.assertionCapabilities.assertEquals(arrayType, invoke, solution)
        } yield assert_stmt
      }
    } yield assert_statements
  }

  // NOTE: I can make generic with CONTEXT but can't remember syntax.
  def map_type_in_class(argType: ArgumentType) : Generator[ooParadigm.ClassContext, Type] = {
    import ooParadigm.classCapabilities._
    import org.combinators.model._

    argType match {
      case _:IntegerType => for {
        tpe <- toTargetLanguageType(TypeRep.Int)
      } yield tpe

      case _:CharType => for {
        tpe <- toTargetLanguageType(TypeRep.Char)
      } yield tpe

      case _:BooleanType => for {
        tpe <- toTargetLanguageType(TypeRep.Boolean)
      } yield tpe

      case _:StringType => for {
        tpe <- toTargetLanguageType(TypeRep.String)
      } yield tpe

      case _:IntegerArrayType => for {
        tpe <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      } yield tpe

      // find which ones need to be implemented
      case _ => ???
    }
  }

  def max_bound_in_method(argExpr: ArgExpression)  : Generator[paradigm.MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import org.combinators.model._

    argExpr.argType match {
      case _:IntegerType => for {
        self <- ooParadigm.methodBodyCapabilities.selfReference()
        field <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle(argExpr.name))
      } yield field

      case _:BooleanType => for {
        self <- ooParadigm.methodBodyCapabilities.selfReference()
        field <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle(argExpr.name))
      } yield field

      case _:StringType => for {
        self <- ooParadigm.methodBodyCapabilities.selfReference()
        field <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle(argExpr.name))
        lengthMethod <- ooParadigm.methodBodyCapabilities.getMember(field, names.mangle("length"))     // bit of a hack for string
        invoke <- paradigm.methodBodyCapabilities.apply(lengthMethod, Seq.empty)
      } yield invoke

      case _:IntegerArrayType => for {
        self <- ooParadigm.methodBodyCapabilities.selfReference()
        field <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle(argExpr.name))
        lengthField <- ooParadigm.methodBodyCapabilities.getMember(field, names.mangle("length"))     // bit of a hack for string
      } yield lengthField

      // find which ones need to be implemented
      case _ => ???
    }
  }

  def map_type_in_method(argType: ArgumentType) : Generator[paradigm.MethodBodyContext, Type] = {
    import paradigm.methodBodyCapabilities._
    import org.combinators.model._

    argType match {
      case _:IntegerType => for {
        tpe <- toTargetLanguageType(TypeRep.Int)
      } yield tpe

      case _:BooleanType => for {
        tpe <- toTargetLanguageType(TypeRep.Int)
      } yield tpe

      case _:CharType => for {
        tpe <- toTargetLanguageType(TypeRep.Char)
      } yield tpe

      case _:StringType => for {
        tpe <- toTargetLanguageType(TypeRep.String)
      } yield tpe

      // find which ones need to be implemented
      case _ => ???
    }
  }

  /**
   * Creates function using parameters from model and returns int:
   *
   *     int SOMEFUNCTION (ARGS)
   *
   * where ARGS represents the model (i.e., (("n", Int)) for Fibonacci and (("s1", String), ("s2", String)) for LCS
   */
  def make_helper_method_signature(args:Seq[ArgExpression]): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    // Type of helper method param is always an integer to refer to earlier subproblem
    //

    for {
      params <- forEach(args) { arg => for {
        argType <- toTargetLanguageType(TypeRep.Int)      // Always will be int since subproblems are ordered
        argName = names.mangle(arg.itArgName)             // use pre-selected iterator
      } yield (argName, argType)
      }
      _ <- setParameters(params)

      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)                         // should always be int, but could be stored in Model
    } yield None
  }


  /**
   * Institute plan for mutual recursion of helper(args) method which calls memo(args) on smaller subproblem, and that
   * method is responsible for computing and storing these subproblems.
   *
   * For Bottom-up solutions, there needs to be a way to access the state storing the previous computations. This is in bottomUp:Option[Expression]
   *
   * As code is generated, there needs to be a way to access information, and that is placed in symbolTable, which maps string (like variable name) to
   * the variable expression (either an argument to the parameter or a locally defined variable).
   *
   * The reason for the map is that there is NO WAY to get the reference to a local variable within coGen; perhaps this lack of capability is for the best
   * since it might otherwise generate code that cannot compile.
   *
   */
  def explore(expr : org.combinators.model.Expression, memoize:Boolean = true, symbolTable: Map[String,Expression], bottomUp:Option[Expression] = None) : Generator[paradigm.MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import AnyParadigm.syntax._

    // turn model Expression into a real expression
    expr match {
      case self:SelfExpression => for {
        neg92 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -92)
        nn2 <- paradigm.methodBodyCapabilities.reify(TypeRep.String, self.variableName)
        e = symbolTable(self.variableName)
      } yield e

      case eq: EqualExpression => for {
        left <- explore(eq.left, memoize, symbolTable, bottomUp)
        right <- explore(eq.right, memoize, symbolTable, bottomUp)
        eq_tpe <- map_type_in_method(eq.tpe)
        e <- eqls.equalityCapabilities.areEqual(eq_tpe, left, right)
      } yield e

      case eq: LessThanExpression => for {
        left <- explore(eq.left, memoize, symbolTable, bottomUp)
        right <- explore(eq.right, memoize, symbolTable, bottomUp)
        e <- arithmetic.arithmeticCapabilities.lt(left, right)
      } yield e

      case eq: LessThanOrEqualExpression => for {
        left <- explore(eq.left, memoize, symbolTable, bottomUp)
        right <- explore(eq.right, memoize, symbolTable, bottomUp)
        e <- arithmetic.arithmeticCapabilities.le(left, right)
      } yield e

      case or:OrExpression => for {
        left <- explore(or.left, memoize, symbolTable, bottomUp)
        right <- explore(or.right, memoize, symbolTable, bottomUp)
        e <- booleans.booleanCapabilities.or(Seq(left, right))
      } yield e

      case or:AndExpression => for {
        left <- explore(or.left, memoize, symbolTable, bottomUp)
        right <- explore(or.right, memoize, symbolTable, bottomUp)
        e <- booleans.booleanCapabilities.and(Seq(left, right))
      } yield e

      case mx:MaxExpression => for {
        left <- explore(mx.left, memoize, symbolTable, bottomUp)
        right <- explore(mx.right, memoize, symbolTable, bottomUp)
        e <- realArithmetic.realArithmeticCapabilities.max(left, right)
      } yield e

      case mn:MinExpression => for {
        left <- explore(mn.left, memoize, symbolTable, bottomUp)
        right <- explore(mn.right, memoize, symbolTable, bottomUp)
        //SHOULD BE CHANGED TO PROPER MIN
        zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
        nLeft <- arithmetic.arithmeticCapabilities.sub(zero,left)
        nRight <- arithmetic.arithmeticCapabilities.sub(zero,right)
        m <- realArithmetic.realArithmeticCapabilities.max(nLeft, nRight)
        e <- arithmetic.arithmeticCapabilities.sub(zero,m)
      } yield e

      // takes "text1" and returns "this.text1"
      case ie:InputExpression => for {
        self <- ooParadigm.methodBodyCapabilities.selfReference()
        e <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle(ie.variableName))
      } yield e

      // StringLengthExpression(new ArgExpression(0)), n))
      case sle:StringLengthExpression => for {
        inner <- explore(sle.string, memoize, symbolTable, bottomUp)
        e <- strings.stringCapabilities.getStringLength(inner)
      } yield e

      case cae:CharAtExpression => for {
        inner <- explore(cae.string, memoize, symbolTable, bottomUp)
        idx <- explore(cae.index, memoize, symbolTable, bottomUp)
        e <- strings.stringCapabilities.getCharAt(inner, idx)
      } yield e

      case ae: SubtractionExpression => for {
        left <- explore(ae.left, memoize, symbolTable, bottomUp)
        right <- explore(ae.right, memoize, symbolTable, bottomUp)
        e <- arithmetic.arithmeticCapabilities.sub(left, right)
      } yield e

      case ae: AdditionExpression => for {
        left <- explore(ae.left, memoize, symbolTable, bottomUp)
        right <- explore(ae.right, memoize, symbolTable, bottomUp)
        e <- arithmetic.arithmeticCapabilities.add(left, right)
      } yield e

      case arge:ArgExpression => for {
        self <- ooParadigm.methodBodyCapabilities.selfReference()
        field <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle(arge.name))
      } yield field

      case alen:ArrayLengthExpression => for {
        inner <- explore(alen.array, memoize, symbolTable, bottomUp)
        len <- ooParadigm.methodBodyCapabilities.getMember(inner, names.mangle("length"))
      } yield len

      case arr:ArrayElementExpression => for {
        // Access array[idx] value
        inner <- explore(arr.array, memoize, symbolTable, bottomUp)
        idx <- explore(arr.index, memoize, symbolTable, bottomUp)
        e <- array.arrayCapabilities.get(inner, idx)
      } yield e

      case me:MultiplicationExpression => for {
        left <- explore(me.left, memoize, symbolTable, bottomUp)
        right <- explore(me.right, memoize, symbolTable, bottomUp)
        e <- arithmetic.arithmeticCapabilities.mult(left, right)
      } yield e

      case ter:TernaryExpression => for {
        cond <- explore(ter.condition, memoize, symbolTable)
        trueBranch <- explore(ter.trueBranch, memoize, symbolTable, bottomUp)
        falseBranch <- explore(ter.falseBranch, memoize, symbolTable, bottomUp)

        intType <- toTargetLanguageType(TypeRep.Int)
        score <- impParadigm.imperativeCapabilities.declareVar(names.mangle("score"), intType, None)

        ifBlock <- impParadigm.imperativeCapabilities.ifThenElse(
          cond,
          for {
            assign <- impParadigm.imperativeCapabilities.assignVar(score, trueBranch)
          } yield assign,
          Seq.empty,
          Some(
            for {
              assign <- impParadigm.imperativeCapabilities.assignVar(score, falseBranch)
            } yield assign
          )
        )
      } yield score

      case se: SubproblemExpression => for {
        self <- ooParadigm.methodBodyCapabilities.selfReference()

        // THIS could be turned into a decorator so we don't have to embed here the memo vs. helper concept another day
        f <- if (memoize) {
          ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("memo"))
        } else {
          ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("helper"))
        }

        all_exprs <- forEach(se.args) { expr => for {
            exp <- explore(expr, memoize, symbolTable, bottomUp)
          } yield exp
        }

        res <- if (!bottomUp.isEmpty) {
          get_matrix_element(bottomUp.get, all_exprs)
        } else {
          paradigm.methodBodyCapabilities.apply(f, all_exprs)
        }

      } yield res

      case hp:HelperExpression => for {
        neg99 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -99)
        def1 = hp.variable
        e = symbolTable(hp.variable)
       } yield e

      case it:IteratorExpression => for {
        actual <- if (bottomUp.isDefined) {
          for {
            args <- paradigm.methodBodyCapabilities.getArguments()   // HACK: needed to start the for lop off.
            e = symbolTable(it.variable)
          } yield e
        } else {
          for {
            args <- paradigm.methodBodyCapabilities.getArguments()
          } yield args(it.iteratorNumber)._3
        }
      } yield actual

      case lit:LiteralInt => for {
        actual <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, lit.literal)
      } yield actual

      case bool:LiteralBoolean => for {
        actual <- paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, bool.literal)
      } yield actual

      case _ => for {   // PLACE HOLDER FOR EVERYTHING ELSE
        zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -99)
      } yield zero
    }
  }

  def create_int_array(values:Seq[Int]) : Generator[MethodBodyContext, Expression] = {
    import AnyParadigm.syntax._
    for {
      translated_vals <- forEach(values) { value =>
        for {
          reified_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, value)
        } yield reified_value
      }

      intType <- paradigm.methodBodyCapabilities.toTargetLanguageType(TypeRep.Int)
      result <- array.arrayCapabilities.create(intType, translated_vals)
    } yield result
  }

  @deprecated("replace with create_array")
  def set_array(sampleVar: Expression, index:Int, values:Seq[Int]) : Generator[MethodBodyContext, Seq[Statement]] = {
    if (values.length == 1) {
      // last one
      for {
        d_i <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, index)
        d_v <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, values.head)
        varIndex <- array.arrayCapabilities.get(sampleVar, d_i)
        a_i <- impParadigm.imperativeCapabilities.assignVar(varIndex, d_v)
      } yield Seq(a_i)
    } else {
      // recursively
      for {
        d_i <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, index)
        d_v <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, values.head)
        varIndex <- array.arrayCapabilities.get(sampleVar, d_i)
        a_i <- impParadigm.imperativeCapabilities.assignVar(varIndex, d_v)
        all_seq <- set_array(sampleVar, index+1, values.tail)
      } yield all_seq :+ a_i
    }
  }

  /**
   * Helper for max operations, represents 'm=max(m,r)'
   * Returns the generated max statement
   */
  def set_max(maxVar: Expression, replacement: Expression): Generator[MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    for {
      maxCond <- arithmetic.arithmeticCapabilities.lt(maxVar, replacement)
      maxIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(maxCond, for {

        assignStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, replacement)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq.empty
      )
    } yield maxIfStmt
  }

  //This code does not work, can't access static method on class
  def full_set_max(maxVar: Expression, e1: Expression, e2: Expression): Generator[MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      mathClass <- findClass(names.mangle("Math"))
      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(mathClass, Seq.empty, None)
      method <- getMember(instantiated, names.mangle("max"))
      maxExp <- apply(method, Seq(e1, e2))
      maxStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, maxExp)
    } yield (maxStmt)
  }

  def new_full_set_max(maxVar: Expression, e1: Expression, e2: Expression): Generator[MethodBodyContext, Seq[Statement]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      set1 <- impParadigm.imperativeCapabilities.assignVar(maxVar, e1)

      intType <- toTargetLanguageType(TypeRep.Int)
      tempName <- freshName(names.mangle("temp"))
      tempVar <- impParadigm.imperativeCapabilities.declareVar(tempName, intType, None)
      tempAssign <- impParadigm.imperativeCapabilities.assignVar(tempVar, e2)

      maxCond <- arithmetic.arithmeticCapabilities.lt(maxVar, tempVar)
      maxIfStmt <- impParadigm.imperativeCapabilities.ifThenElse(maxCond, for {
        assignStmt <- impParadigm.imperativeCapabilities.assignVar(maxVar, tempVar)
        _ <- addBlockDefinitions(Seq(assignStmt))
      } yield (),
        Seq.empty
      )
    } yield Seq(set1, tempAssign, maxIfStmt)
  }

  def plus_equals(variable: Expression, value: Expression): Generator[MethodBodyContext, Statement] = {
    for {
      addExpr <- arithmetic.arithmeticCapabilities.add(variable, value)
      assign <- impParadigm.imperativeCapabilities.assignVar(variable, addExpr)
    } yield assign
  }

  def char_at(string: Expression, index: Expression): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      method <- getMember(string, names.mangle("charAt"))
      char_at <- apply(method, Seq(index))
    } yield char_at
  }

  def declare_and_inst_variable(varName: String, varType: Type, varValueGenerator: Generator[paradigm.MethodBodyContext, Expression]): Generator[paradigm.MethodBodyContext, Expression] = {
    for {
      varValue <- varValueGenerator
      outputVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(varName), varType, Some(varValue))
    } yield outputVar
  }

  def declare_and_inst_variable(varName: String, varType: Type, varValue: Expression): Generator[paradigm.MethodBodyContext, Expression] = {
    for {
      outputVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle(varName), varType, Some(varValue))
    } yield outputVar
  }

  def make_for_loop(iterator: Expression, guard: Expression, update: Expression, body: Seq[Statement]): Generator[paradigm.MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      while_loop <- impParadigm.imperativeCapabilities.whileLoop(
        guard,
        for {
          _ <- addBlockDefinitions(body)
          updated_iterator <- impParadigm.imperativeCapabilities.assignVar(iterator, update)
          _ <- addBlockDefinitions(Seq(updated_iterator))
        } yield ()
      )
    } yield while_loop
  }

  def make_for_loop(iterator: Expression, guard: Expression, body: Seq[Statement]): Generator[paradigm.MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)

      while_loop <- impParadigm.imperativeCapabilities.whileLoop(guard, for {
        _ <- addBlockDefinitions(body)

        incrExpr <- arithmetic.arithmeticCapabilities.add(iterator, one)
        incrStmt <- impParadigm.imperativeCapabilities.assignVar(iterator, incrExpr)
        _ <- addBlockDefinitions(Seq(incrStmt))
      } yield ())

      //_ <- addBlockDefinitions(Seq(while_loop))

    } yield while_loop
  }

  def get_matrix_element(matrix: Expression, indices:Seq[Expression]): Generator[paradigm.MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    if (indices.length == 1) {
      for {
        ai <- array.arrayCapabilities.get(matrix, indices.head)
      } yield ai
    } else if (indices.length == 2) {
      for {
        ai <- array.arrayCapabilities.get(matrix, indices.head)
        aij <- array.arrayCapabilities.get(ai, indices.tail.head)
      } yield aij
    } else if (indices.length == 3) {
      for {
        ai <- array.arrayCapabilities.get(matrix, indices.head)
        aij <- array.arrayCapabilities.get(ai, indices.tail.head)
        aijk <- array.arrayCapabilities.get(aij, indices.tail.tail.head)
      } yield aijk
    } else if (indices.length == 3) {
      for {
        ai <- array.arrayCapabilities.get(matrix, indices.head)
        aij <- array.arrayCapabilities.get(ai, indices.tail.head)
        aijk <- array.arrayCapabilities.get(aij, indices.tail.tail.head)
        aijkl <- array.arrayCapabilities.get(aij, indices.tail.tail.tail.head)
      } yield aijkl
    } else {
      ???
    }
  }

  def get_matrix_element(matrix: Expression, row: Expression, col: Expression): Generator[paradigm.MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      matrix_at_r <- array.arrayCapabilities.get(matrix, row)
      matrix_at_r_c <- array.arrayCapabilities.get(matrix_at_r, col)
    } yield matrix_at_r_c
  }

  /**
   * Helper function for nested for loops.
   *
   * @param var1          An expression representing the variable to be updated in the outer loop.
   * @param guard1        An expression representing the guard condition for the outer loop.
   * @param update1       An expression representing the update expression for the outer loop.
   * @param var2          An expression representing the variable to be updated in the inner loop.
   * @param guard2        An expression representing the guard condition for the inner loop.
   * @param update2       An expression representing the update expression for the inner loop.
   * @param inner_body    The body of the inner loop.
   * @param trailing_body Additional statements to be executed at the end of the inner loop after the execution of the inner loop.
   * @return A generator of statements for the outer loop.
   */
  def make_nested_for_loop(var1: Expression, guard1: Expression, update1: Expression,
                           var2: Expression, guard2: Expression, update2: Expression,
                           inner_body: Seq[Statement], trailing_body: Seq[Statement]): Generator[paradigm.MethodBodyContext, Statement] = {
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      outer_loop <- impParadigm.imperativeCapabilities.whileLoop(guard1,
        for {
          inner_loop <- impParadigm.imperativeCapabilities.whileLoop(guard2,
            for {
              _ <- addBlockDefinitions(inner_body)

              updateStmt <- impParadigm.imperativeCapabilities.assignVar(var2, update2)
              _ <- addBlockDefinitions(Seq(updateStmt))
            } yield ()
          )

          _ <- addBlockDefinitions(Seq(inner_loop))

          _ <- addBlockDefinitions(trailing_body)

          updateStmt <- impParadigm.imperativeCapabilities.assignVar(var1, update1)
          _ <- addBlockDefinitions(Seq(updateStmt))
        } yield ()
      )
    } yield outer_loop
  }
}
