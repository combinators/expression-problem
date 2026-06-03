package org.combinators.boilerplate.twoSequences

import org.combinators.dp.TestExample
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{AbstractSyntax, NameProvider, TypeRep}
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, Maps, RealArithmetic, Strings}
import org.combinators.cogen.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.models.{EnhancedModel, LiteralBoolean, LiteralExpression, LiteralInt, LiteralString}
import org.combinators.dp.enhanced.{BottomUpStrategy, EnhancedDPProvider, EnhancedUtility, TopDownStrategy}
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait WordBreakProvider extends EnhancedDPProvider with EnhancedUtility with TopDownStrategy with BottomUpStrategy {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val realArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext ,paradigm.type]
  val maps: Maps.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  // if not memo, then this will be defined and added
  lazy val resultVarName = names.mangle("result")

  import paradigm._
  import syntax._

  // expand as necessary
  def literalMapping(litExpr:LiteralExpression): Generator[MethodBodyContext, Expression] = {

    // only SINGLE values can go here
    litExpr match {
      case lit: LiteralInt => paradigm.methodBodyCapabilities.reify(TypeRep.Int, lit.literal)
      case bool: LiteralBoolean => paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, bool.literal)
      case str: LiteralString => paradigm.methodBodyCapabilities.reify(TypeRep.String, str.literal)

      case _ => ??? // error in all other circumstances
    }
  }

  def wordBreakTests(implementation:String, tests: Seq[TestExample]): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      assert_statements <- forEach(tests) { test =>

        val output = test.answer match {
          case lit:LiteralBoolean => lit.literal
          case _ => ???
        }

        // Arrays have to be handled specially, I'm afraid
        val values = test.inputType match {
          case wb:WordBreakInput => (wb.s, wb.dictionary)
          case _ => ???
        }

        for {
          solType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle(implementation))
          s <- paradigm.methodBodyCapabilities.reify(TypeRep.String, values._1)
          dict <- create_string_array(values._2)

          sol <- ooParadigm.methodBodyCapabilities.instantiateObject(solType, Seq(s, dict))

          computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, computeName)

          sol_actual <- apply(computeMethod, Seq.empty)
          sol_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Boolean, output)
          theType <- toTargetLanguageType(TypeRep.Boolean)
          asserteq_fib <- asserts.assertionCapabilities.assertEquals(theType, sol_actual, sol_value)

        } yield asserteq_fib
      }
    } yield assert_statements
  }

  def makeTestCase(clazzName:String, tests:Seq[TestExample]): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(wordBreakTests(clazzName, tests), names.mangle(clazzName))
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

object WordBreakProvider {
  type WithParadigm[P <: AnyParadigm] = WordBreakProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   ffiArithmetic: Arithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   ffiRealArithmetic: RealArithmetic.WithBase[base.MethodBodyContext, base.type, Double],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   maps: Maps.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   stringsIn: Strings.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   parametricPolymorphism: ParametricPolymorphism.WithBase[base.type],
   booleansIn: Booleans.WithBase[base.MethodBodyContext, base.type]
  )
  (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): WordBreakProvider.WithParadigm[base.type] =
    new WordBreakProvider {
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
      override val maps: Maps.WithBase[base.MethodBodyContext, paradigm.type] = maps
      override val mapsInConstructors = ???
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val strings: Strings.WithBase[base.MethodBodyContext, paradigm.type] = stringsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val booleans: Booleans.WithBase[base.MethodBodyContext, paradigm.type] = booleansIn
    }
}
