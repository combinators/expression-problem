package org.combinators.archive.unenhancedModels.boilerplate.uncrossedLines

import org.combinators.archive.unenhancedModels.models.LiteralArrayPair
import org.combinators.dp.TestExample
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, RealArithmetic, Strings}
import org.combinators.cogen.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.cogen.{AbstractSyntax, NameProvider, TypeRep}
import org.combinators.dp.original.DPObjectOrientedProvider
import org.combinators.models.{LiteralInt, UnitExpression}

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait UnenhancedUncrossedLinesProvider extends DPObjectOrientedProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]

  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val realArithmetic: RealArithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm._
  import syntax._

  // Specific examples hard coded for Int input and Int output
  def makeTests(implementation:String, tests: Seq[TestExample] = Seq.empty): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._

    // NOTE: these tests are in the wrong place, since we defer test gen to later
    val tests = Seq(
      new TestExample("ucl0", LiteralArrayPair(Array(1, 4, 2), Array(1, 2, 4)), LiteralInt(2), new UnitExpression), // https://leetcode.com/problems/uncrossed-lines/
      new TestExample("ucl1", LiteralArrayPair(Array(2, 5, 1, 2, 5), Array(10, 5, 2, 1, 5, 2)), LiteralInt(3), new UnitExpression), // https://leetcode.com/problems/uncrossed-lines/
    )

    for {
      assert_statements <- forEach(tests) { test =>

        val input_value = test.inputType match {
          case lt: LiteralArrayPair => (lt.ar1, lt.ar2)
          case _ => ??? // error in all other circumstances
        }

        val expected_value = test.answer match {
          case lit:LiteralInt => lit.literal
          case _ => ???
        }

        for {
          fibType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle(implementation))

          arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))

          ar1 <- create_int_array(input_value._1)
          ar2 <- create_int_array(input_value._2)

          sol <- ooParadigm.methodBodyCapabilities.instantiateObject(fibType, Seq(ar1, ar2))
          
          computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, computeName)

          intType <- toTargetLanguageType(TypeRep.Int)
          fibn_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, expected_value)
          fib_actual <- apply(computeMethod, Seq.empty)
          asserteq_fib <- asserts.assertionCapabilities.assertEquals(intType, fib_actual, fibn_value)

        } yield asserteq_fib
      }
    } yield assert_statements
  }


  override def makeTestCase(implementation:String): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTests(implementation), names.mangle("DP"))
    } yield ()
  }
}

object UncrossedLinesProvider {
  type WithParadigm[P <: AnyParadigm] = DPObjectOrientedProvider { val paradigm: P }
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
           (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): UncrossedLinesProvider.WithParadigm[base.type] =
    new UnenhancedUncrossedLinesProvider {
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
