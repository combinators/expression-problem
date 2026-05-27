package org.combinators.helloworld

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AnyParadigm, FindClass, ObjectOriented}
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, Maps}
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
  val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int]

  val console: Console.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext,paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val maps: Maps.WithBase[paradigm.MethodBodyContext, paradigm.type]

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
      paramName <- freshName(names.mangle(fieldName))            // make sure to create a unique name for param, to avoid name clashes with field
      _ <- setParameters(Seq((paramName,stringType)))
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
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
     // msg <- array.arrayCapabilities.get(args.head._3, Seq(zero))

     // res <- instantiateObject(worldType, Seq(msg))
     // fname <- freshName(names.mangle("msg"))  // be sure to unpack since this has a side effect on the context....
     // fvar <- declareVar(fname, worldType, Some(res))

     // msgMethod <- getMember(fvar, names.mangle(getter(message)))
     // result <- apply(msgMethod, Seq.empty)
     // output <- console.consoleCapabilities.print(result)
    //  le <- liftExpression(output)
    //  _ <- addBlockDefinitions(Seq(le))

      // array example
      intType <- toTargetLanguageType(TypeRep.Int)
      arr1Type <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      arr2Type <- toTargetLanguageType(TypeRep.Array(TypeRep.Array(TypeRep.Int)))
      arname <- freshName(names.mangle("ar"))

      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      three <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 3)
      initial <- array.arrayCapabilities.create(intType, Seq(two, three), None)
      A <- declareVar(arname, arr2Type, Some(initial))

      // now set individual values
      setInst1 <- array.arrayCapabilities.set(A, Seq(zero, one), three)       // A[0][1] = 3
      setStmt1 <- impParadigm.imperativeCapabilities.liftExpression(setInst1)

      len1 <- array.arrayCapabilities.length(A, Seq.empty)  // first dimension
      len2 <- array.arrayCapabilities.length(A, Seq(zero))  // 2nd dimension
      len1Sub1 <-  ffiArithmetic.arithmeticCapabilities.sub(len1, one)
      len2Sub1 <-  ffiArithmetic.arithmeticCapabilities.sub(len2, one)
      setInst2 <- array.arrayCapabilities.set(A, Seq(len1Sub1, len2Sub1), two) // A[A.length-1][A[0].length-1] = 2
      setStmt2 <- impParadigm.imperativeCapabilities.liftExpression(setInst2)
      _ <- addBlockDefinitions(Seq(setStmt1, setStmt2))

      // showcase maps
      stringType <- toTargetLanguageType(TypeRep.String)
      initialMap <- maps.mapCapabilities.create(stringType, intType)
      mapType <- toTargetLanguageType(TypeRep.Map(TypeRep.String, TypeRep.Int))
        mapVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("myMap"), mapType, Some(initialMap))

      expr1 <- paradigm.methodBodyCapabilities.reify(TypeRep.Map(TypeRep.String,TypeRep.Int), Map[String,Int]("hello" -> 42) )
      mapVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("myMap2"), mapType, Some(expr1))

      // put something
      newKey <- paradigm.methodBodyCapabilities.reify(TypeRep.String, "there")
      newValue <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 88)

      expr2 <- maps.mapCapabilities.put(mapVar, newKey, newValue)
      mapVar <- impParadigm.imperativeCapabilities.declareVar(names.mangle("myMap3"), mapType, Some(expr2))

    } yield None // Some(res)
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
   ffi1:  Arithmetic.WithBase[base.MethodBodyContext, base.type, Int],
   con: Console.WithBase[base.MethodBodyContext, base.type],
   arr: Arrays.WithBase[base.MethodBodyContext, base.type],
   assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
   eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],
   mapsIn: Maps.WithBase[base.MethodBodyContext, base.type]
  )
  : HelloWorldObjectOrientedProvider.WithParadigm[base.type] =
    new HelloWorldObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      override val names: NameProvider[paradigm.syntax.Name] = nameProvider
      override val ooParadigm: ObjectOriented.WithBase[paradigm.type] = oo
      override val ffiArithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Int] = ffi1

      override val console: Console.WithBase[base.MethodBodyContext, paradigm.type] = con
      override val array: Arrays.WithBase[base.MethodBodyContext, paradigm.type] = arr
      override val asserts: Assertions.WithBase[base.MethodBodyContext, paradigm.type] = assertsIn
      override val eqls: Equality.WithBase[base.MethodBodyContext, paradigm.type] = eqlsIn
      override val maps: Maps.WithBase[base.MethodBodyContext, paradigm.type] = mapsIn
    }
}
