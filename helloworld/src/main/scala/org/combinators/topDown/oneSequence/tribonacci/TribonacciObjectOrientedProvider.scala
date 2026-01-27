package org.combinators.topDown.oneSequence.tribonacci

import org.combinators.dp.{BottomUp, GenerationOption, TopDown, Utility}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.Generics.WithBase
import org.combinators.ep.generator.paradigm.ParametricPolymorphism.WithBase
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality, Strings}
import org.combinators.ep.generator.paradigm.{AnyParadigm, FindClass, Generics, ObjectOriented, ParametricPolymorphism}
import org.combinators.ep.generator.{AbstractSyntax, Command, NameProvider, Understands}
import org.combinators.model.Model

trait TribonacciObjectOrientedProvider extends TribonacciProvider with Utility {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  val names: NameProvider[paradigm.syntax.Name]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val arithmetic: Arithmetic.WithBase[paradigm.MethodBodyContext, paradigm.type, Double]
  val console: Console.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val array: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val asserts: Assertions.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val strings: Strings.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  lazy val message: String = "message"
  lazy val main: String = "main"
  lazy val testName = names.mangle("TestSuite")
  lazy val dpName = names.mangle("dp")
  lazy val memoName = names.mangle("memo")

  lazy val rName = names.mangle("r")
  lazy val cName = names.mangle("c")

  var memo: Boolean = false

  lazy val resultVarName = names.mangle("result")

  def getter(attr: String): String = {
    "get" + attr.capitalize
  }

  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(Seq(names.mangle(names.conceptNameOf(dtpe)))).interpret(canFindClass)
  }

  //  def registerTypeMapping(tpe: DataType): Generator[ProjectContext, Unit] = {
  //    import ooParadigm.projectCapabilities.{addTypeLookupForClasses, addTypeLookupForConstructors}
  //    import paradigm.projectCapabilities.addTypeLookupForMethods
  //
  //    val dtpe = TypeRep.DataType(tpe)
  //
  //    for {
  //      _ <- addTypeLookupForMethods(dtpe, domainTypeLookup(tpe))
  //      _ <- addTypeLookupForClasses(dtpe, domainTypeLookup(tpe))
  //      _ <- addTypeLookupForConstructors(dtpe, domainTypeLookup(tpe))
  //    } yield ()
  //  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  def make_compute_method_signature(): Generator[paradigm.MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      intType <- toTargetLanguageType(TypeRep.Int)
      _ <- setParameters(Seq((names.mangle("n"), intType)))
      _ <- setReturnType(intType)

    } yield ()
  }

  def make_compute_method(): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._

    for {
      _ <- make_compute_method_signature()
      args <- getArguments()

      self <- ooParadigm.methodBodyCapabilities.selfReference()
      func <- ooParadigm.methodBodyCapabilities.getMember(self, names.mangle("compute"))

      (name, tpe, n) = args.head

      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      two <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 2)
      three <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 3)

      neq0 <- arithmetic.arithmeticCapabilities.le(n, zero)
      neq2 <- arithmetic.arithmeticCapabilities.le(n, two)

      ifStmt <- impParadigm.imperativeCapabilities.ifThenElse(
        neq0,
        for {
          returnStmt <- impParadigm.imperativeCapabilities.returnStmt(zero)
          _ <- addBlockDefinitions(Seq(returnStmt))
        } yield (),
        Seq(
          (neq2,
            for {
              returnStmt <- impParadigm.imperativeCapabilities.returnStmt(one)
              _ <- addBlockDefinitions(Seq(returnStmt))
            } yield ()),
        ),
        Some(
          for {
            n_1 <- arithmetic.arithmeticCapabilities.sub(n, one)
            fn_1 <- apply(func, Seq(n_1))
            n_2 <- arithmetic.arithmeticCapabilities.sub(n, two)
            fn_2 <- apply(func, Seq(n_2))
            n_3 <- arithmetic.arithmeticCapabilities.sub(n, three)
            fn_3 <- apply(func, Seq(n_3))

            out <- arithmetic.arithmeticCapabilities.add(fn_1, fn_2)
            out <- arithmetic.arithmeticCapabilities.add(out, fn_3)
            returnStmt <- impParadigm.imperativeCapabilities.returnStmt(out)
            _ <- addBlockDefinitions(Seq(returnStmt))
          } yield ()
        )
      )

      _ <- addBlockDefinitions(Seq(ifStmt))
    } yield None
  }

  def makeSimpleDP(): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        _ <- addMethod(names.mangle("compute"), make_compute_method())
      } yield None
    }

    addClassToProject(makeClass, names.mangle("Tribonacci"))
  }

  //  todo: make test cases
  //  def makeTestCase(): Generator[MethodBodyContext, Seq[Expression]] = {
  //    import paradigm.methodBodyCapabilities._
  //    import eqls.equalityCapabilities._
  //
  //    for {
  //      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
  //    } yield ()
  //  }

  def implement(model: Model, option: GenerationOption): Generator[ProjectContext, Unit] = {
//
//    var isTopDown = false
//
//    option match {
//      case td: TopDown =>
//        memo = td.memo
//        isTopDown = true
//      case _: BottomUp =>
//        isTopDown = false
//    }
//
//    for {
//      _ <- if (isTopDown) {
//        make_top_down(model)
//      } else {
//        make_bottom_up(model)
//      }
//    } yield ()

    for {
      _ <- makeSimpleDP()
      //      _ <- paradigm.projectCapabilities.addCompilationUnit(
      //        paradigm.compilationUnitCapabilities.addTestSuite(testName, makeTestCase("DP"))
      //      )
    } yield None
  }
}

object TribonacciObjectOrientedProvider {
  type WithParadigm[P <: AnyParadigm] = TribonacciObjectOrientedProvider {val paradigm: P}
  type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

  def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
           (base: P)
           (nameProvider: NameProvider[base.syntax.Name],
            imp: Imperative.WithBase[base.MethodBodyContext, base.type],
            ffiArithmetic: Arithmetic.WithBase[base.MethodBodyContext, base.type, Double],
            con: Console.WithBase[base.MethodBodyContext, base.type],
            arr: Arrays.WithBase[base.MethodBodyContext, base.type],
            assertsIn: Assertions.WithBase[base.MethodBodyContext, base.type],
            stringsIn: Strings.WithBase[base.MethodBodyContext, base.type],
            eqlsIn: Equality.WithBase[base.MethodBodyContext, base.type],oo: ObjectOriented.WithBase[base.type],
            parametricPolymorphism: ParametricPolymorphism.WithBase[base.type])
           (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): TribonacciObjectOrientedProvider.WithParadigm[base.type] =
    new TribonacciObjectOrientedProvider {
      override val paradigm: base.type = base
      val impParadigm: imp.type = imp
      val arithmetic: ffiArithmetic.type = ffiArithmetic
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