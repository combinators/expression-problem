package org.combinators.ep.language.scala.codegen

/*DI:LD:AI*/

import cats.{Apply => _}
import org.combinators.ep.generator.{Command, FileWithPath, Understands}
import org.combinators.ep.language.scala.{Finalized, ScalaNameProvider}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.{Apply, ToTargetLanguageType}
import org.combinators.ep.language.inbetween.any.{AbstractSyntax, AnyParadigm, Method, Project, Type}
import org.combinators.ep.language.inbetween.oo.{Class, Constructor, OOParadigm}
import org.combinators.ep.language.inbetween.imperative.Imperative
import org.combinators.ep.language.inbetween.ffi.{Arithmetic, Booleans, Equals, Lists, Strings}
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphism
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics

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

  def toLookup[Ctxt](name: String): Option[Generator[Ctxt, Type[Finalized.FinalTypes]]] = {
    Some(Command.lift(factory.classReferenceType(nameProvider.mangle(name))))
  }

  def addLookupsForImplementedGenerators[Ctxt](
    project: Project[Finalized.FinalTypes],
    add: (Project[Finalized.FinalTypes], TypeRep => Option[Generator[Ctxt, Type[Finalized.FinalTypes]]]) => Project[Finalized.FinalTypes]
  )(implicit canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[Type[Finalized.FinalTypes]]],
    canApplyType: Understands[Ctxt, Apply[Type[Finalized.FinalTypes], Type[Finalized.FinalTypes], Type[Finalized.FinalTypes]]]
  ): Project[Finalized.FinalTypes] = {
    add(project, {
      case TypeRep.Double => toLookup("Double")
      case TypeRep.String => toLookup("String")
      case TypeRep.Unit => toLookup("Unit")
      case TypeRep.Sequence(elemTpeRep) =>
        Some(
          for {
            elemTpe <- ToTargetLanguageType[Type[Finalized.FinalTypes]](elemTpeRep).interpret(canToTargetLanguage)
            seqTpe <- Command.lift(factory.classReferenceType(nameProvider.mangle("Seq")))
            tpe <- Apply[
              Type[Finalized.FinalTypes],
              Type[Finalized.FinalTypes],
              Type[Finalized.FinalTypes]](seqTpe, Seq(elemTpe)).interpret(canApplyType)
          } yield tpe)
      case _ => None
    })
  }
  def runGenerator(generator: Generator[Project[Finalized.FinalTypes], Unit]): Seq[FileWithPath] = {
    var projectWithLookups = factory.ooProject()
    projectWithLookups =
      addLookupsForImplementedGenerators[Method[Finalized.FinalTypes]](
        projectWithLookups,
        { case (project, lookup) => factory.convert(project).addTypeLookupsForMethods(lookup) }
      )(paradigm.methodBodyCapabilities.canTransformTypeInMethodBody,
        parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod)
    projectWithLookups =
      addLookupsForImplementedGenerators[Class[Finalized.FinalTypes]](
        projectWithLookups,
        { case (project, lookup) => factory.convert(project).addTypeLookupsForClasses(lookup) }
      )(ooParadigm.classCapabilities.canTranslateTypeInClass,
        generics.classCapabilities.canApplyTypeInClass)
    projectWithLookups =
      addLookupsForImplementedGenerators[Constructor[Finalized.FinalTypes]](
        projectWithLookups,
        { case (project, lookup) => factory.convert(project).addTypeLookupsForConstructors(lookup) }
      )(ooParadigm.constructorCapabilities.canTranslateTypeInConstructor,
        generics.constructorCapabilities.canApplyTypeInConstructor)

    Command.runGenerator(generator, projectWithLookups)._1.compilationUnits.map(cu => FileWithPath(factory.convert(cu).toScala, {
      val nameAsStrings = cu.name.map(name => factory.convert(name).toScala)
      val nameWithScalaExtension = nameAsStrings.init :+ (nameAsStrings.last + ".scala")

      nameWithScalaExtension.foldLeft(Paths.get("src"))({ case (path, name) =>
        Paths.get(path.toString, name)
      })
    })).toSeq
  }

  val paradigm = AnyParadigm[Finalized.FinalTypes, factory.type, syntax.type](factory, runGenerator, syntax)
  val ooParadigm = OOParadigm[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  val imperative = Imperative[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  val parametricPolymorphism = ParametricPolymorphism[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  val generics = Generics[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)(ooParadigm, parametricPolymorphism)

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
    */

  val listsInMethod = Lists[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  /*val listsInConstructor =
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
