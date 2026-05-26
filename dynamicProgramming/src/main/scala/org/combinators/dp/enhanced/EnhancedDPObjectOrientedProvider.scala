package org.combinators.dp.enhanced

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{AbstractSyntax, NameProvider, TypeRep}
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.cogen.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.dp.TestExample
import org.combinators.models.{EnhancedModel, LiteralArray, LiteralArrayPair, LiteralBoolean, LiteralChar, LiteralExpression, LiteralInt, LiteralPair, LiteralString, LiteralStringPair, LiteralStringTriple, LiteralTriple}

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

  // expand as necessary
  def literalMapping(litExpr:LiteralExpression): Generator[MethodBodyContext, Expression] = {

    // only SINGLE values can go here
    litExpr match {
      case lit: LiteralInt => paradigm.methodBodyCapabilities.reify(TypeRep.Int, lit.literal)
      case bool: LiteralBoolean => paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, bool.literal)
      case str: LiteralString => paradigm.methodBodyCapabilities.reify(TypeRep.String, str.literal)
      case chr: LiteralChar => paradigm.methodBodyCapabilities.reify(TypeRep.Char, chr.literal)

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
          case _:LiteralChar => toTargetLanguageType(TypeRep.Char)
          case _:LiteralBoolean => toTargetLanguageType(TypeRep.Boolean)
          case _:LiteralString => toTargetLanguageType(TypeRep.String)
          case _:LiteralTriple => toTargetLanguageType(TypeRep.Int)          // triple means three separate integer values
          case _:LiteralPair => toTargetLanguageType(TypeRep.Int)
          case _ => ???
        }

        // Arrays have to be handled specially, I'm afraid
        val createArray = test.inputType match {
          case _:LiteralArray =>
            true
          case _:LiteralArrayPair => true
          case _ => false
        }

        // if > 0 then this is a sequence of parameters
        val sequenceLength = test.inputType match {
          case lt:LiteralTriple => Seq(lt.val1, lt.val2, lt.val3)
          case lp:LiteralPair => Seq(lp.val1, lp.val2)
          case _ => Seq.empty
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
          sol <- if (createArray) {
            val vals = test.inputType match {
              case la:LiteralArray => Seq(la.literal)
              case la:LiteralArrayPair => Seq(la.ar1, la.ar2)
              case _ => Seq.empty
            }

            val dimensions = test.inputType match {
              case la:LiteralArray =>
                la.dimensions
              case lap:LiteralArrayPair =>
                Seq(lap.ar1.length)      // these are two one-dimensional arrays that must be same length.
              case _ => Seq.empty
            }

            val type_rep = dimensions.length match {
              case 1 => TypeRep.Array(TypeRep.Int)
              case 2 => TypeRep.Array(TypeRep.Array(TypeRep.Int))
              case 3 => TypeRep.Array(TypeRep.Array(TypeRep.Array(TypeRep.Int)))
              case _ =>  ???
            }

            if (vals.length == 1) {
              for {
                arrayType <- toTargetLanguageType(type_rep)
                expr <- create_int_nd_array(vals.head, dimensions)
                variable <- impParadigm.imperativeCapabilities.declareVar(names.mangle(test.name), arrayType, Some(expr))
                sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, Seq(variable))
              } yield sol
            } else if (vals.length == 2) {
              for {
                arrayType <- toTargetLanguageType(type_rep)
                expr1 <- create_int_nd_array(vals.head, dimensions)
                expr2 <- create_int_nd_array(vals.tail.head, dimensions)
                var1 <- impParadigm.imperativeCapabilities.declareVar(names.mangle(test.name + "1"), arrayType, Some(expr1))
                var2 <- impParadigm.imperativeCapabilities.declareVar(names.mangle(test.name + "2"), arrayType, Some(expr2))
                sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, Seq(var1, var2))
              } yield sol
            } else {
              ???
            }
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
            if (sequenceLength.isEmpty) {
              for {
                litval <- literalMapping(test.inputType)
                sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, Seq(litval))
              } yield sol
            } else if(sequenceLength.length == 2) {
              for {
                arg1 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, sequenceLength(0))
                arg2 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, sequenceLength(1))
                sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, Seq(arg1, arg2))
              } yield sol
            } else if (sequenceLength.length == 3) {
              for {
                arg1 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, sequenceLength(0))
                arg2 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, sequenceLength(1))
                arg3 <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, sequenceLength(2))
                sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, Seq(arg1, arg2, arg3))
              } yield sol
            } else {
              ???
            }
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
    for {
      _ <- paradigm.testCapabilities.addTestCase(genericTests(clazzName, tests), names.mangle(clazzName))
    } yield ()
  }

  /** Trying out some new capabilities */
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
        paradigm.compilationUnitCapabilities.addTestSuite(names.mangle("Test" + model.problem), makeTestCase(model.problem, tests)),
        names.mangle("Test" + model.problem)
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
