package org.combinators.equals

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Booleans, Console, Equality, Maps}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.equals.ffi.BaseType

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 *
 */
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
  val bases:  BaseType.WithBase[paradigm.MethodBodyContext, paradigm.type]   // .WithBase have the context
  val eqlGenerator : EqualsGenerator.Aux[paradigm.type, ooParadigm.type]

  import paradigm._
  import syntax._
  import ooParadigm._

  lazy val message:String = "message"
  lazy val main:String = "main"
  lazy val testName = names.mangle("TestSuite")

  def makeTestMethod(): Generator[ClassContext, Unit] = {
    for {
      _ <- Command.skip
    } yield ()
  }

  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import eqls.equalityCapabilities._
    val eq2 = eqls.equalityCapabilities.canEquals
    val  not2 = booleans.booleanCapabilities.canNot

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      stringType <- toTargetLanguageType(TypeRep.String)
      pointType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("Point"))
      rectangleType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("Rectangle"))
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      three <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 3)

      pt1 <- ooParadigm.methodBodyCapabilities.instantiateObject(pointType, Seq(one, two))
      pt2 <- ooParadigm.methodBodyCapabilities.instantiateObject(pointType, Seq(one, two))
      pt3 <- ooParadigm.methodBodyCapabilities.instantiateObject(pointType, Seq(two, three))

      rect1 <- ooParadigm.methodBodyCapabilities.instantiateObject(rectangleType, Seq(one, two, pt1))
      rect2 <- ooParadigm.methodBodyCapabilities.instantiateObject(rectangleType, Seq(one, two, pt1))
      rect3 <- ooParadigm.methodBodyCapabilities.instantiateObject(rectangleType, Seq(one, two, pt3))

      asserteq1 <- asserts.assertionCapabilities.assertEquals(pointType, pt1, pt2)

      // cannot get assertNotEquals to work without providing implicits
      asserteq2 <- asserts.assertionCapabilities.assertNotEquals(pointType, pt1, pt3)(eq2, not2)

      asserteq3 <- asserts.assertionCapabilities.assertEquals(rectangleType, rect1, rect2)

      // cannot get assertNotEquals to work without providing implicits
      asserteq4 <- asserts.assertionCapabilities.assertNotEquals(rectangleType, rect1, rect3)(eq2, not2)

    } yield Seq(asserteq1, asserteq2, asserteq3, asserteq4)
  }

  def makeTestCase(clazzName:String): Generator[TestContext, Unit] = {
    for {
      _ <- paradigm.testCapabilities.addTestCase(makeTestCase(), names.mangle(clazzName))
    } yield ()
  }

  def implement(domains:Seq[DataType]) : Generator[ProjectContext, Unit] = {
    import AnyParadigm.syntax._

    // Just grab CompositeDataType
    val composites = domains.collect {
      case comp:CompositeDataType => comp
    }

    for {
      _ <- forEach(composites) { domain => for {
          _ <- eqlGenerator.generateSkeleton(domain)
        } yield ()
      }

      _ <- forEach(composites) { domain => for {
        _ <- paradigm.projectCapabilities.addCompilationUnit(
          paradigm.compilationUnitCapabilities.addTestSuite(
            testName, makeTestCase(domain.name)
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
