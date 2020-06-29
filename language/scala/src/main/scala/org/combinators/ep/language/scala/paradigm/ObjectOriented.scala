package org.combinators.ep.language.scala.paradigm   /*DI:LD:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{ObjectOriented => OO, AnyParadigm => _, _}
import org.combinators.ep.language.scala.Syntax.MangledName
import org.combinators.ep.language.scala.{ClassCtxt, CompilationUnitCtxt, ContextSpecificResolver, CtorCtxt, MethodBodyCtxt, ScalaNameProvider, TestCtxt}

import scala.meta._
import org.combinators.ep.language.scala


trait ObjectOriented[AP <: AnyParadigm] extends OO {
  val base: AP
  import base.syntax._
  import base.config

  type ClassContext = ClassCtxt
  type ConstructorContext = CtorCtxt

  val compilationUnitCapabilities: CompilationUnitCapabilities =
    new CompilationUnitCapabilities {
      implicit val canAddClassInCompilationUnit: Understands[CompilationUnitCtxt, AddClass[ClassCtxt, MangledName]] =
        new Understands[CompilationUnitCtxt, AddClass[ClassCtxt, MangledName]] {
          def perform(
            context: CompilationUnitCtxt,
            command: AddClass[ClassCtxt, MangledName]
          ): (CompilationUnitCtxt, Unit) = {
            val clsPrototype =
              Defn.Class(
                List.empty,
                Type.Name(command.name.toAST.value),
                List.empty,
                Ctor.Primary(List.empty, Name.Anonymous(), List.empty),
                Template(
                  List.empty,
                  List.empty,
                  Self(Name.Anonymous(), None),
                  List.empty
                )
              )
            val (commandCtxt, _) =
              Command.runGenerator(
                command.cls,
                ClassCtxt(context.resolver, clsPrototype, Seq.empty)
              )

            val updatedUnit: Source = {
              val oldUnit = context.unit
              val imports =
                (context.unit.stats.collect {
                  case imp: Import => imp
                } ++ commandCtxt.extraImports).distinct.sortBy(_.toString)

              val nextUnit =
                List(
                  imports,
                  oldUnit.stats.filter(!_.isInstanceOf[Import]),
                  List(commandCtxt.cls)
                ).flatten

              Source(nextUnit)
            }
            (context.copy(resolver = commandCtxt.resolver, unit = updatedUnit), ())
          }
        }
    }
  val classCapabilities: ClassCapabilities =
    new ClassCapabilities {
      implicit val canDebugInClass: Understands[ClassCtxt, Debug] =
        new Understands[ClassCtxt, Debug] {
          def perform(context: ClassCtxt, command: Debug): (ClassCtxt, Unit) = {
            System.err.println(command.tag + ": " + context.cls)
            (context,())
          }
        }
      implicit val canAddTypeLookupForMethodsInClass: Understands[ClassCtxt, AddTypeLookup[MethodBodyCtxt, Type]] =
        new Understands[ClassCtxt, AddTypeLookup[MethodBodyCtxt, Type]] {
          def perform(context: ClassCtxt, command: AddTypeLookup[MethodBodyCtxt, Type]): (ClassCtxt, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[MethodBodyCtxt, Type] =
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._methodTypeResolution(k)(tpe)
              }
            (context.copy(resolver = context.resolver.copy(_methodTypeResolution = newLookup)), ())
          }
        }
      implicit val canAddTypeLookupForClassesInClass: Understands[ClassCtxt, AddTypeLookup[ClassCtxt, Type]] =
        new Understands[ClassCtxt, AddTypeLookup[ClassCtxt, Type]] {
          def perform(context: ClassCtxt, command: AddTypeLookup[ClassCtxt, Type]): (ClassCtxt, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[ClassCtxt, Type] =
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._classTypeResolution(k)(tpe)
              }
            (context.copy(resolver = context.resolver.copy(_classTypeResolution = newLookup)), ())
          }
        }
      implicit val canAddTypeLookupForConstructorsInClass: Understands[ClassCtxt, AddTypeLookup[CtorCtxt, Type]] =
        new Understands[ClassCtxt, AddTypeLookup[CtorCtxt, Type]] {
          def perform(context: ClassCtxt, command: AddTypeLookup[CtorCtxt, Type]): (ClassCtxt, Unit) = {
            def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[CtorCtxt, Type] =
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._constructorTypeResolution(k)(tpe)
              }
            (context.copy(resolver = context.resolver.copy(_constructorTypeResolution = newLookup)), ())
          }
        }
      implicit val canAddParentInClass: Understands[ClassCtxt, AddParent[Type]] =
        new Understands[ClassCtxt, AddParent[Type]] {
          def perform(context: ClassCtxt, command: AddParent[Type]): (ClassCtxt, Unit) = {
            def updateTemplate(templ: Template): Template =
              templ.copy(
                inits = templ.inits :+ Init(command.parentClass, Name.Anonymous(), List.empty)
              )
            (context.copy(cls = ObjectOriented.patchClass(context.cls, updateTemplate)), ())
          }
        }
      implicit val canAddImplementedInClass: Understands[ClassCtxt, AddImplemented[Type]] =
        new Understands[ClassCtxt, AddImplemented[Type]] {
          def perform(context: ClassCtxt, command: AddImplemented[Type]): (ClassCtxt, Unit) = {
            def updateTemplate(templ: Template): Template =
              templ.copy(
                inits = templ.inits :+ Init(command.interface, Name.Anonymous(), List.empty)
              )
            (context.copy(cls = ObjectOriented.patchClass(context.cls, updateTemplate)), ())
          }
        }
      implicit val canRemoveMethodFromClass: Understands[ClassCtxt, RemoveMethod[Type, MangledName]] =
        new Understands[ClassCtxt, RemoveMethod[Type, MangledName]] {
          def perform(context: ClassCtxt, command: RemoveMethod[Type, MangledName]): (ClassCtxt, Unit) = {
            def updateTemplate(templ: Template): Template =
              templ.copy(stats = templ.stats.filter { stat =>
                stat match {
                  case defn: Defn.Def => defn.name.structure != command.name.toAST.structure
                  case decl: Decl.Def => decl.name.structure != command.name.toAST.structure
                  case _ => true
                }
              })
            (context.copy(cls = ObjectOriented.patchClass(context.cls, updateTemplate)), ())
          }
        }
      implicit val canAddFieldInClass: Understands[ClassCtxt, AddField[MangledName, Type]] = ???
      implicit val canGetFieldInClass: Understands[ClassCtxt, GetField[MangledName, Term]] = ???
      implicit val canAddMethodInClass: Understands[ClassCtxt, AddMethod[MethodBodyCtxt, MangledName, Option[Term]]] = ???
      implicit val canAddConstructorInClass: Understands[ClassCtxt, AddConstructor[CtorCtxt]] = ???
      implicit val canAddImportInClass: Understands[ClassCtxt, AddImport[Import]] = ???
      implicit val canResolveImportInClass: Understands[ClassCtxt, ResolveImport[Import, Type]] = ???
      implicit val canSetAbstractInClass: Understands[ClassCtxt, SetAbstract] = ???
      implicit val canSetStaticInClass: Understands[ClassCtxt, SetStatic] = ???
      implicit val canSetInterfaceInClass: Understands[ClassCtxt, SetInterface] = ???
      implicit val canTranslateTypeInClass: Understands[ClassCtxt, ToTargetLanguageType[Type]] = ???
      implicit val canSelfReferenceInClass: Understands[ClassCtxt, SelfReference[Term]] = ???
      implicit val canFindClassInClass: Understands[ClassCtxt, FindClass[MangledName, Type]] = ???
      implicit val canGetFreshNameInClass: Understands[ClassCtxt, FreshName[MangledName]] = ???
    }
  val constructorCapabilities: ConstructorCapabilities = ???
  val methodBodyCapabilities: MethodBodyCapabilities = ???
  val projectCapabilities: ProjectCapabilities = ???
  val testCapabilities: TestCapabilities = ???
}

object ObjectOriented {
  def patchClass(cls: Defn, patch: Template => Template): Defn = {
    cls match {
      case cls: Defn.Class =>
        cls.copy(templ = patch(cls.templ))
      case trt: Defn.Trait =>
        trt.copy(templ = patch(trt.templ))
      case obj: Defn.Object =>
        obj.copy(templ = patch(obj.templ))
    }
  }
}