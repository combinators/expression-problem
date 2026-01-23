package org.combinators.dp

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.dp.Utility
import org.combinators.model.{AdditionExpression, EqualExpression, FunctionExpression, IteratorExpression, LiteralInt, Model, SubproblemExpression, SubtractionExpression}

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait DPObjectOrientedProvider extends DPProvider with Utility {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
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

  // ,
  //               elseBranch:Option[Generator[paradigm.MethodBodyContext, Seq[Statement]]]
  def iftemplate(guard:Expression,
               ifBranch:Generator[paradigm.MethodBodyContext, Seq[Statement]])
    : Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- impParadigm.imperativeCapabilities.ifThenElse(guard, for {
        stmts <- ifBranch
        _ <- addBlockDefinitions(stmts)
      } yield (), Seq.empty)
    } yield ()
  }

  def explore(expr : org.combinators.model.Expression) : Generator[paradigm.MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import AnyParadigm.syntax._
    // turn model Expression into a real expression
    expr match {
      case eq: EqualExpression => for {
        left <- explore(eq.left)
        right <- explore(eq.right)
        intType <- toTargetLanguageType(TypeRep.Int)
        e <- eqls.equalityCapabilities.areEqual(intType, left, right)
      } yield e

      case ae: SubtractionExpression => for {
        left <- explore(ae.left)
        right <- explore(ae.right)
        e <- arithmetic.arithmeticCapabilities.sub(left, right)
      } yield e

      case ae: AdditionExpression => for {
        left <- explore(ae.left)
        right <- explore(ae.right)
        e <- arithmetic.arithmeticCapabilities.add(left, right)
      } yield e

      case se: SubproblemExpression => for {
        self <- ooParadigm.methodBodyCapabilities.selfReference()
        f <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("helper"))
        allexprs <- forEach(se.args) { expr => for {
            exp <- explore(expr)
          } yield exp
        }
        res <- paradigm.methodBodyCapabilities.apply(f, allexprs)
      } yield res

      case it:IteratorExpression => for {
        args <- paradigm.methodBodyCapabilities.getArguments()
      } yield args(it.iteratorNumber)._3

      case lit:LiteralInt => for {
        actual <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, lit.literal)
      } yield actual

      case _ => for {   // PLACE HOLDER
        zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      } yield zero
   }
  }

  def assignResult (variable:Expression, expr:org.combinators.model.Expression): Unit = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import AnyParadigm.syntax._

    for {
      expr_result <- explore(expr)
      stmt <- impParadigm.imperativeCapabilities.assignVar(variable, expr_result)
      _ <- addBlockDefinitions(Seq(stmt))
    } yield ()
  }

  def make_helper_method(model:Model): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import AnyParadigm.syntax._

    val real_cases = model.cases.filter(p => p._1.isDefined)
    val first_case = real_cases.head
    val tail_cases = real_cases.tail.seq
    val elseCase = model.cases.filter(p => p._1.isEmpty)   // MUST only be one

    for {
      _ <- make_compute_method_signature()
      intType <- toTargetLanguageType(TypeRep.Int)
      result <- impParadigm.imperativeCapabilities.declareVar(names.mangle("result"), intType)
      _ <- setReturnType(intType)
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      helperMethod <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("helper"))
      nfield <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("n"))
      inner <- explore(first_case._1.get)

      all_rest <- forEach(tail_cases) { next_case =>
        for {
          next_cond <- explore(next_case._1.get)
          next_exp <- explore(next_case._2)
        } yield (next_cond, expand(result, next_exp))
      }

      ifstmt <- impParadigm.imperativeCapabilities.ifThenElse(inner
        ,
        for {
          resexp <- explore(first_case._2)
          av <- impParadigm.imperativeCapabilities.assignVar(result, resexp)
          _ <- addBlockDefinitions(Seq(av))
        } yield None
        ,
        all_rest
        ,
        Some(for {
          resexp <- explore(elseCase.head._2)
          av <- impParadigm.imperativeCapabilities.assignVar(result, resexp)
          _ <- addBlockDefinitions(Seq(av))
        } yield ())
      )
      _ <- addBlockDefinitions(Seq(ifstmt))

    } yield Some(result)

  }

  private def expand(result: Expression, nextexp: paradigm.syntax.Expression): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import AnyParadigm.syntax._
    for {
      av <- impParadigm.imperativeCapabilities.assignVar(result, nextexp)
      _ <- addBlockDefinitions(Seq(av))
    } yield None
  }

  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import AnyParadigm.syntax._
    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setReturnType(intType)
      self <- ooParadigm.methodBodyCapabilities.selfReference()
      helperMethod <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("helper"))
      field <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("n"))
      invocation <- apply(helperMethod, Seq(field))
    } yield Some(invocation)

  }

  def createConstructor(n:String): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)     // should't just assume this

      _ <- setParameters(Seq((names.mangle(n), intType)))
      args <- getArguments()
      _ <- initializeField(names.mangle(n), args.head._3)

    } yield ()
  }

  def makeTopDown(model:Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        intType <- toTargetLanguageType(TypeRep.Int)    // shouldn't be hard-coded: should be able to infer from model
        _ <- addField(names.mangle("n"), intType )
        _ <- addConstructor(createConstructor("n"))
        _ <- addMethod(names.mangle("helper"), make_helper_method(model))
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle(model.problem))
  }


  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import eqls.equalityCapabilities._

    // can only be optimized if 'forEach' is added to CoGen. Right now I think it is in EpCoGen
    val initial_vals = Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)

    for {
      fibType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("Fibonacci"))
      d_7 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 7)
      sol <- ooParadigm.methodBodyCapabilities.instantiateObject(fibType, Seq(d_7))
      computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, names.mangle("compute"))
      intType <- toTargetLanguageType(TypeRep.Int)
      //sampleVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("sample"), arrayType, Some(oo1))

       // allAssigns <- set_array(sampleVar, 0, initial_vals)
      d_13 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 13)
      fib_7 <- apply(computeMethod, Seq.empty)
      asserteq8 <- asserts.assertionCapabilities.assertEquals(intType, fib_7, d_13)

    } yield Seq(asserteq8)
  }

  def makeTestCase(clazzName:String): Generator[TestContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTestCase(), names.mangle(clazzName))
    } yield ()
  }

  def implement(model:Model): Generator[ProjectContext, Unit] = {

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
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : DPObjectOrientedProvider.WithParadigm[base.type] =
    new DPObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
    }
}
