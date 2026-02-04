package org.combinators.helloworld

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arrays, Assertions, Console, Equality}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{AbstractSyntax, Command, NameProvider, Understands}

/** Any OO approach will need to properly register type mappings and provide a default mechanism for finding a class
 * in a variety of contexts. This trait provides that capability
 *
 */
trait HelloWorldObjectOrientedProvider extends HelloWorldProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext,paradigm.type]
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

  def makeSignature() : Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      stringType <- toTargetLanguageType(TypeRep.String)
      _ <- setReturnType(stringType)
      _ <- resolveAndAddImport(stringType)
    } yield ()
  }

  def methodImplementation(): Generator[MethodBodyContext, Option[Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    for {
      _ <- makeSignature()
      self <- selfReference()
      welcomeMsg <- getMember(self, names.mangle(message))

      // body of this implementation is the result of the individual domain-specific logic.
    } yield Some(welcomeMsg)
  }

  def createConstructor(fieldName:String): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    for {
      stringType <- toTargetLanguageType(TypeRep.String)

      _ <- setParameters(Seq((names.mangle(fieldName),stringType)))
      args <- getArguments()
      _ <- initializeField(names.mangle(fieldName), args.head._3)

    } yield ()
  }

  /** Make a field from an attribute in the given class.  If the type needs to be different from default, then register Types accordingly. */
  def createField(fieldName:String): Generator[ClassContext, Type] = {
    import ooParadigm.classCapabilities._
    for {
      stringType <- toTargetLanguageType(TypeRep.String)
      _ <- resolveAndAddImport(stringType)
      _ <- addField(names.mangle(fieldName), stringType)
    } yield stringType
  }

  def makeClass(clazzName:String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- createField (message)
        _ <- addConstructor(createConstructor (message))
        _ <- addMethod(names.mangle(getter(message)), methodImplementation())   // HACK
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(clazzName))
  }

  def makeStaticSignature() : Generator[MethodBodyContext, Unit] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      _ <- setStatic()
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.String))
      _ <- resolveAndAddImport(arrayType)
      unitType <- toTargetLanguageType(TypeRep.Unit)
      _ <- setReturnType(unitType)
      _ <- setParameters(Seq((names.mangle("args"), arrayType)) )
    } yield ()
  }

  def staticMethodImplementation(): Generator[MethodBodyContext, Option[Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    import impParadigm.imperativeCapabilities._

    for {
      _ <- makeStaticSignature()
      worldType <- findClass(names.mangle("World"))
      _ <- resolveAndAddImport(worldType)
      args <- getArguments()
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      msg <- array.arrayCapabilities.get(args.head._3, zero)

      res <- instantiateObject(worldType, Seq(msg))
      fname <- freshName(names.mangle("msg"))  // be sure to unpack since this has a side effect on the context....
      fvar <- declareVar(fname, worldType, Some(res))

      msgMethod <- getMember(fvar, names.mangle(getter(message)))
      result <- apply(msgMethod, Seq.empty)
      output <- console.consoleCapabilities.print(result)
      le <- liftExpression(output)
      _ <- addBlockDefinitions(Seq(le))

    } yield Some(res)
  }

  def makeMainClass(clazzName:String): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle(main), staticMethodImplementation())
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(clazzName))
  }

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
      msgMethod <- ooParadigm.methodBodyCapabilities.getMember(res, names.mangle(getter(message)))
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

  def implement(): Generator[ProjectContext, Unit] = {

    for {
      _ <- makeClass("World")
      _ <- makeMainClass("Main")
      _ <- paradigm.projectCapabilities.addCompilationUnit(
        paradigm.compilationUnitCapabilities.addTestSuite(
          testName, makeTestCase("World")
        )
      )
    } yield ()
  }
}

object HelloWorldObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = HelloWorldObjectOrientedProvider { val paradigm: P }
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
  (base: P)
  (nameProvider: NameProvider[base.syntax.Name],
   imp: Imperative.WithBase[base.MethodBodyContext, base.type],
   oo: ObjectOriented.WithBase[base.type],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type]
  )
  : HelloWorldObjectOrientedProvider.WithParadigm[base.type] =
    new HelloWorldObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
    }
}
