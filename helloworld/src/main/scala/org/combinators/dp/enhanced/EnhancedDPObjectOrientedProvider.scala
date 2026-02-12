package org.combinators.dp.enhanced

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{AbstractSyntax, NameProvider}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.model.{EnhancedModel, LiteralArray, LiteralBoolean, LiteralExpression, LiteralInt, LiteralString, LiteralStringPair, LiteralStringTriple}
import org.combinators.dp._

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait EnhancedDPObjectOrientedProvider extends EnhancedDPProvider with EnhancedUtility with TopDownStrategy with BottomUpStrategy {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val realArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  // if not memo, then this will be defined and added
  lazy val resultVarName = names.mangle("result")

  import paradigm._
  import syntax._
  import ooParadigm._


  /**
   * Method creates a compute() method with no arguments that invokes helper method:
   *
   *   public Integer compute() {
   *     return this.helper(this.n);
   *   }
   */
  def make_compute_method(model:EnhancedModel): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    // make list of params in proper order
    val orderedParams = model.solution.order.filter(varName => model.solution.parameters.contains(varName))

    for {
      returnType <- return_type_based_on_model(model)
      _ <- setReturnType(returnType)

      self <- ooParadigm.methodBodyCapabilities.selfReference()
      helperMethod <- ooParadigm.methodBodyCapabilities.getMember(self, helperName)

      // convert arguments into Iteration values
      args <- forEach(orderedParams) { varName => for {
        neg77 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, -77)
        pair = model.solution.parameters(varName)

          // arg1 <- max_bound_in_method(boundExpr)
        arg1 <- explore(pair._1, memoize = false, symbolTable = Map.empty)  // At this point, there should be no symbols
        } yield arg1
      }
      //field <- ooParadigm.methodBodyCapabilities.getMember(self, nName)
      invocation <- apply(helperMethod, args)
    } yield Some(invocation)
  }

  // expand as necessary
  def literalMapping(litExpr:LiteralExpression): Generator[MethodBodyContext, Expression] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._

    // only SINGLE values can go here
    litExpr match {
      case lit: LiteralInt => paradigm.methodBodyCapabilities.reify(TypeRep.Int, lit.literal)
      case bool: LiteralBoolean => paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, bool.literal)
      case str: LiteralString => paradigm.methodBodyCapabilities.reify(TypeRep.String, str.literal)

      case _ => ??? // error in all other circumstances
    }
  }

  def genericTests(implementation:String, tests: Seq[TestExample]): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      assert_statements <- forEach(tests) { test =>

        val the_test_type = test.answer match {
          case _:LiteralInt => toTargetLanguageType(TypeRep.Int)
          case _:LiteralBoolean => toTargetLanguageType(TypeRep.Boolean)
          case _:LiteralString => toTargetLanguageType(TypeRep.String)
          case _ => ???
        }

        // Arrays have to be handled specially, I'm afraid
        val createArray = test.inputType match {
          case _:LiteralArray => true
          case _ => false
        }

        val createStrings = test.inputType match {
          case _:LiteralStringTriple => true
          case _:LiteralStringPair => true
          case _ => false
        }

        val sol_gen_value = test.answer match {
          case lit:LiteralInt => paradigm.methodBodyCapabilities.reify(TypeRep.Int, lit.literal)
          case bool:LiteralBoolean => paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, bool.literal)
          case _ => ???
        }

        for {
          solType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle(implementation))
          // sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, value)
          //
          sol <- if (createArray) {
            val vals = test.inputType match {
              case la:LiteralArray => la.ar
              case _ => Array.empty
            }

            for {
              arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))

              expr <- create_int_array(vals)
              variable <- impParadigm.imperativeCapabilities.declareVar(names.mangle(test.name), arrayType, Some(expr))
              sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, Seq(variable))
            } yield sol
          } else if (createStrings) {
            val vals = test.inputType match {
              case triple:LiteralStringTriple => Seq(triple.string1, triple.string2, triple.string3)
              case pair:LiteralStringPair => Seq(pair.string1, pair.string2)
              case _ => Seq.empty
            }

            for {
              all <- forEach (vals) { v1 =>
                for {
                  v1_val <- paradigm.methodBodyCapabilities.reify(TypeRep.String, v1)
                } yield v1_val
              }
              sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, all)
            } yield sol
          } else {
            for {
              litval <- literalMapping(test.inputType)
              sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, Seq(litval))
            } yield sol
          }

          computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, computeName)

          sol_actual <- apply(computeMethod, Seq.empty)
          sol_value <- sol_gen_value
          theType <- the_test_type
          asserteq_fib <- asserts.assertionCapabilities.assertEquals(theType, sol_actual, sol_value)

        } yield asserteq_fib
      }
    } yield assert_statements
  }

  def makeTestCase(clazzName:String, tests:Seq[TestExample]): Generator[TestContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
      _ <- paradigm.testCapabilities.addTestCase(genericTests(clazzName, tests), names.mangle(clazzName))
    } yield ()
  }

  /** Trying out some new capabiltiies */
  def implement(model: EnhancedModel, tests:Seq[TestExample], option:GenerationOption): Generator[ProjectContext, Unit] = {

    // handle Top/Bottom and properly set memo when TD
    var isTopDown = false
    var useMemo = false
    option match {
      case td:TopDown =>
        useMemo = td.memo
        isTopDown = true

      case _:BottomUp =>
        isTopDown = false
    }

    for {
      // The code below generates the actual class, based on model.problem
      _ <- if (isTopDown) {
        make_top_down(useMemo, model)
      } else {
        make_bottom_up(model)
      }

      _ <- paradigm.projectCapabilities.addCompilationUnit(
        paradigm.compilationUnitCapabilities.addTestSuite(names.mangle("Test" + model.problem), makeTestCase(model.problem, tests))
      )
    } yield ()
  }
}

object EnhancedDPObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = EnhancedDPObjectOrientedProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   ffiArithmetic: Arithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   stringsIn: Strings.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   parametricPolymorphism: ParametricPolymorphism.WithBase[base.type],
   booleansIn: Booleans.WithBase[base.MethodBodyContext, base.type]
  )
  (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): EnhancedDPObjectOrientedProvider.WithParadigm[base.type] =
    new EnhancedDPObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
      val realArithmetic: ffiRealArithmetic.type = ffiRealArithmetic
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: oo.type = oo
      override val polymorphics: parametricPolymorphism.type = parametricPolymorphism
      override val genericsParadigm: generics.type = generics
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val strings: Strings.WithBase[base.MethodBodyContext, paradigm.type] = stringsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val booleans: Booleans.WithBase[base.MethodBodyContext, paradigm.type] = booleansIn
    }
}