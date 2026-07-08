package org.combinators.equals

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, Maps}
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

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      stringType <- toTargetLanguageType(TypeRep.String)
      worldType <- ooParadigm.methodBodyCapabilities.findClass(names.mangle("World"))

      msg <- paradigm.methodBodyCapabilities.reify(TypeRep.String, "Hey There!")
      res <- ooParadigm.methodBodyCapabilities.instantiateObject(worldType, Seq(msg))
      msgMethod <- ooParadigm.methodBodyCapabilities.getMember(res, names.mangle("getSomething"))
      result <- apply(msgMethod, Seq.empty)
      asserteq1 <- asserts.assertionCapabilities.assertEquals(stringType, result, msg)

    } yield Seq(asserteq1)
  }

  def makeTestCase(clazzName:String): Generator[TestContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
        _ <- paradigm.testCapabilities.addTestCase(makeTestCase(), names.mangle(clazzName))
      } yield ()
  }

  def implement(domain:CompositeDataType) : Generator[ProjectContext, Unit] = {

    for {
      _ <- eqlGenerator.generateSkeleton(domain)
      _ <- paradigm.projectCapabilities.addCompilationUnit(
        paradigm.compilationUnitCapabilities.addTestSuite(
          testName, makeTestCase(domain.name)
        )
      )
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

      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val maps: Maps.WithBase[base.MethodBodyContext, paradigm.type] = mapsIn
      override val bases: BaseType.WithBase[base.MethodBodyContext, paradigm.type] = baseTypeIn
      override val eqlGenerator: EqualsGenerator.Aux[paradigm.type, ooParadigm.type] = equalGeneratorIn
    }
}
