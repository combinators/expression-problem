package org.combinators.ep.language.scala.codegen

/*DI:LD:AI*/

import cats.{Apply => _}
import org.combinators.ep.generator.{Command, FileWithPath}
import org.combinators.ep.language.scala.{Finalized, ScalaNameProvider}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.language.inbetween.any.{AbstractSyntax, AnyParadigm, Project}
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.imperative.Imperative
import org.combinators.ep.language.inbetween.ffi.Booleans
import org.combinators.ep.language.inbetween.ffi.Arithmetic
import org.combinators.ep.language.inbetween.ffi.Strings
import org.combinators.ep.language.inbetween.ffi.Equals

import java.nio.file.{Path, Paths}

/**
 * Java-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
sealed class CodeGenerator { cc =>
  val factory = new Finalized.Factory {}


  val syntax: AbstractSyntax[Finalized.FinalTypes] = new AbstractSyntax[Finalized.FinalTypes] {}
  val nameProvider = new ScalaNameProvider[Finalized.FinalTypes](factory)
  def runGenerator(generator: Generator[Project[Finalized.FinalTypes], Unit]): Seq[FileWithPath] = {
    val project = factory.project().addTypeLookupForMethods(TypeRep.Double, Command.lift(factory.classReferenceType(nameProvider.mangle("Double"))))
    Command.runGenerator(generator,project)._1.compilationUnits.map(cu => FileWithPath(factory.convert(cu).toScala,
      cu.name.foldLeft(Paths.get("src"))({ case (path, name) =>
        Paths.get(path.toString, factory.convert(name).toScala)
      }))).toSeq
  }

  val paradigm = AnyParadigm[Finalized.FinalTypes, factory.type, syntax.type](factory, runGenerator, syntax)
  val ooParadigm = OOParadigm[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  val imperative = Imperative[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  //val parametricPolymorphism: ParametricPolymorphism[paradigm.type] = ParametricPolymorphism(paradigm)
  //val generics: Generics.Aux[paradigm.type, ooParadigm.type, parametricPolymorphism.type] = Generics(paradigm)(ooParadigm, parametricPolymorphism)

  val booleans = Booleans[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  val doubles = Arithmetic[Finalized.FinalTypes, factory.type, paradigm.type, Double](paradigm)
  /*val realDoublesInMethod =
    new RealArithmetic[MethodBodyCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      PrimitiveType.doubleType(),
      new DoubleLiteralExpr(_)
    )
  val realDoublesInConstructor =
    new RealArithmetic[MethodBodyCtxt, Double, paradigm.type](
      paradigm,
      TypeRep.Double,
      PrimitiveType.doubleType(),
      new DoubleLiteralExpr(_)
    )*/

  val ints = Arithmetic[Finalized.FinalTypes, factory.type, paradigm.type, Int](paradigm)

  val strings = Strings[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  val equality = Equals[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  /*val consoleInMethod =
    new Console[MethodBodyCtxt, paradigm.type](
      paradigm, stringsInMethod
    )

  val consoleInConstructor =
    new Console[CtorCtxt, paradigm.type](
      paradigm, stringsInConstructor
    )
  
  val arraysInMethod =
    new Arrays[MethodBodyCtxt, paradigm.type](
      paradigm
    )

  val arraysInConstructor =
    new Arrays[CtorCtxt, paradigm.type](
      paradigm
    )
    

  val listsInMethod =
    Lists[MethodBodyCtxt, paradigm.type, generics.type](
      paradigm,
      ooParadigm.methodBodyCapabilities.canGetMemberInMethod,
      paradigm.methodBodyCapabilities.canApplyInMethodBody,
      parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod,
      paradigm.methodBodyCapabilities.canAddImportInMethodBody
    )(generics)

  val listsInConstructor =
    Lists[CtorCtxt, paradigm.type, generics.type](
      paradigm,
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor,
      generics.constructorCapabilities.canApplyTypeInConstructor,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(generics)

  val treesInMethod =
    Trees[MethodBodyCtxt, paradigm.type, ObjectOriented](
      paradigm,
      paradigm.methodBodyCapabilities.canAddImportInMethodBody
    )(ooParadigm)

  val treesInConstructor =
    Trees[CtorCtxt, paradigm.type, ObjectOriented](
      paradigm,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(ooParadigm)

  val assertionsInMethod = new Assertions[paradigm.type](paradigm)(ooParadigm)
  val exceptionsInMethod = new Exceptions[paradigm.type](paradigm)*/
}

object CodeGenerator {

  case object Enable extends Command {
    type Result = Unit
  }


  def apply(): CodeGenerator =
    new CodeGenerator()
}
