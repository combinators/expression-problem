package org.combinators.oneInteger.fibonacci

import org.combinators.dp.{DPObjectOrientedProvider, TestExample}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.forEach
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, RealArithmetic, Strings}
import org.combinators.ep.generator.paradigm.{AnyParadigm, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.{AbstractSyntax, NameProvider}
import org.combinators.model.{LiteralInt, UnitExpression}

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 */
trait FibonacciProvider extends DPObjectOrientedProvider {
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

  import paradigm._
  import syntax._

  // Specific examples hard coded for Int input and Int output
  def makeTestsFibonacci(implementation:String, tests: Seq[TestExample] = Seq.empty): Generator[MethodBodyContext, Seq[Expression]] = {
    import eqls.equalityCapabilities._
    import paradigm.methodBodyCapabilities._

    val tests = Seq(
      new TestExample("fib0", new LiteralInt(0), 0, new UnitExpression), // for now, leave solution as None
      new TestExample("fib1", new LiteralInt(1), 1, new UnitExpression),
      new TestExample("fib2", new LiteralInt(2), 1, new UnitExpression),
      new TestExample("fib7", new LiteralInt(7), 13, new UnitExpression),
      new TestExample("fib20", new LiteralInt(20), 6765, new UnitExpression),
      new TestExample("fib40", new LiteralInt(40), 102334155, new UnitExpression)
    )

    for {
      assert_statements <- forEach(tests) { example =>

        val input_value = example.inputType match {
          case lt: LiteralInt => lt.literal
          case _ => ??? // error in all other circumstances
        }

        for {
          fibType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle(implementation))
          n_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, input_value)
          sol <- ooParadigm.methodBodyCapabilities.instantiateObject(fibType, Seq(n_value))
          computeMethod <- ooParadigm.methodBodyCapabilities.getMember(sol, computeName)

          intType <- toTargetLanguageType(TypeRep.Int)
          fibn_value <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, example.answer)
          fib_actual <- apply(computeMethod, Seq.empty)
          asserteq_fib <- asserts.assertionCapabilities.assertEquals(intType, fib_actual, fibn_value)

        } yield asserteq_fib
      }
    } yield assert_statements
  }


  override def makeTestCase(implementation:String): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTestsFibonacci(implementation), names.mangle("DP"))
    } yield ()
  }
}

object FibonacciProvider {
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
   parametricPolymorphism: ParametricPolymorphism.WithBase[base.type])
  (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): FibonacciProvider.WithParadigm[base.type] =
    new FibonacciProvider {
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
    }
}
