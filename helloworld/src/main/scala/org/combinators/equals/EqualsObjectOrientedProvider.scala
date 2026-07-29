package org.combinators.equals

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, Maps}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{AbstractSyntax, Command, NameProvider}
import org.combinators.equals.ffi.BaseType
import org.combinators.equals.ffi.BaseType.CompositeTpe

trait EqualsObjectOrientedProvider extends EqualsProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val console : Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val maps: Maps.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val bases:  BaseType.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqlGenerator : EqualsGenerator.Aux[paradigm.type, ooParadigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val testName = names.mangle("TestSuite")

  def makeTestMethod(): Generator[ClassContext, Unit] = {
    for {
      _ <- Command.skip
    } yield ()
  }

  def makeTestCase(testCases:Seq[EqualsTestCase]): Generator[MethodBodyContext, Seq[Expression]] = {
    import paradigm.methodBodyCapabilities.{reify => _, _}
    import eqls.equalityCapabilities._
    import AnyParadigm.syntax._
    import booleans.booleanCapabilities.canNot

    for {
      allTests <- forEach(testCases) { test => for {
          objectType <- toTargetLanguageType(test.object1.tpe)
          obj1 <- reify(test.object1)
          obj2 <- reify(test.object2)
          assertion <- if (test.expected) {
            asserts.assertionCapabilities.assertEquals(objectType, obj1, obj2)
          } else {
            asserts.assertionCapabilities.assertNotEquals(objectType, obj1, obj2)
          }

        } yield assertion
      }
    } yield allTests
  }

  def makeTestCase(clazzName:String, testCases:Seq[EqualsTestCase]): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTestCase(testCases), names.mangle(clazzName))
    } yield ()
  }

  def implement(domains:Seq[CompositeDataType],
                testCases:Seq[EqualsTestCase]) : Generator[ProjectContext, Unit] = {
    import AnyParadigm.syntax._

    

    for {
      _ <- forEach(domains) { domain => for {
          _ <- eqlGenerator.generateSkeleton(domain)
        } yield ()
      }

      _ <- forEach(domains) { domain => for {
        _ <- paradigm.projectCapabilities.addCompilationUnit(
          paradigm.compilationUnitCapabilities.addTestSuite(
            testName, makeTestCase(domain.name, testCases)
          )
        )
      } yield () }
    } yield ()
  }
}

object EqualsObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = EqualsObjectOrientedProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   ffi1:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   booleansIn: Booleans.WithBase[ base.MethodBodyContext, base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   mapsIn: Maps.WithBase[base.MethodBodyContext, base.type],
   baseTypeIn: BaseType.WithBase[base.MethodBodyContext, base.type],
   equalGeneratorIn: EqualsGenerator.Aux[base.type, oo.type]
  )
  : EqualsObjectOrientedProvider.WithParadigm[base.type] =
    new EqualsObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: oo.type = oo
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = ffi1
      override val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type] = booleansIn
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val maps: Maps.WithBase[base.MethodBodyContext, paradigm.type] = mapsIn
      override val bases: BaseType.WithBase[base.MethodBodyContext, paradigm.type] = baseTypeIn
      override val eqlGenerator: EqualsGenerator.Aux[paradigm.type, ooParadigm.type] = equalGeneratorIn
    }
}
