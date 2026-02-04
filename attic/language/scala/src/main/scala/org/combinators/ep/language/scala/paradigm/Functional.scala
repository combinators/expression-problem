package org.combinators.ep.language.scala.paradigm   /*DI:LD:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.paradigm.{AddImport, AddMethod, AddType, AddTypeConstructor, AddTypeLookup, FindMethod, FindType, InstantiateType, ResolveImport, ToTargetLanguageType, Functional => Func}
import org.combinators.ep.language.scala.{Syntax, TypeCtxt, _}
import org.combinators.ep.generator.Understands
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Command

import scala.meta._
import scala.util.Try

trait Functional[AP <: AnyParadigm] extends Func {
  val base: AP
  import base.syntax._

  type TypeContext = TypeCtxt 

  object compilationUnitCapabilities extends CompilationUnitCapabilities {
    implicit val canAddTypeInCompilationUnit: Understands[CompilationUnitCtxt, AddType[Name, TypeContext]] =
      new Understands[CompilationUnitCtxt, AddType[Name, TypeContext]] {
        def perform(context: CompilationUnitCtxt, command: AddType[Syntax.MangledName, TypeCtxt]): (CompilationUnitCtxt, Unit) = {
          def emptyEnum(name: scala.meta.Type.Name): scala.meta.Defn.Class = {
            scala.meta.Defn.Class(
              mods = List.empty,
              name = name,
              tparams = List.empty,
              ctor = Ctor.Primary(mods = List.empty, name = Name.Anonymous(), paramss = List.empty),
              templ = Template(
                early = List.empty,
                inits = List.empty,
                self = Self(Name.Anonymous(), None),
                stats = List.empty
              )
            )
          }

          val (commandCtxt, _) = 
                Command.runGenerator(
                  command.tpeGen,
                  TypeCtxt(context.resolver, emptyEnum, Seq.empty)
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
                  List(commandCtxt.tpe(scala.meta.Type.Name(command.name.toAST.value)))
                ).flatten

              Source(nextUnit)
            }
            (context.copy(resolver = commandCtxt.resolver, unit = updatedUnit), ())
          }
      }

      implicit val canAddMethodInCompilationUnit: Understands[CompilationUnitCtxt, AddMethod[MethodBodyCtxt, Name, Expression]] = 
        new Understands[CompilationUnitCtxt, AddMethod[MethodBodyCtxt, Name, Expression]] {
          def perform(
            context: CompilationUnitCtxt,
            command: AddMethod[MethodBodyCtxt, Name, Term]
          ): (CompilationUnitCtxt, Unit) = {
            val (methodCtxt, resultExp) =
              Command.runGenerator(
                command.spec,
                MethodBodyCtxt(
                  resolver = context.resolver,
                  extraImports = Seq.empty,
                  method = Defn.Def(
                    mods = List.empty,
                    name = Term.Name(command.name.toAST.value),
                    tparams = List.empty,
                    paramss = List.empty,
                    decltpe = None,
                    body = Term.Block(List.empty)
                  )
                )
              )

            val methodToAdd =
              methodCtxt.method match {
                case defn: Defn.Def =>
                  defn.copy(
                    body = defn.body match {
                      case Term.Block(stats) => Term.Block(stats :+ resultExp)
                      case other => Term.Block(List(other, resultExp))
                    }
                  )
                case decl: Decl.Def =>
                  Defn.Def(
                    mods = decl.mods,
                    name = decl.name,
                    tparams = decl.tparams,
                    paramss = decl.paramss,
                    decltpe = Some(decl.decltpe),
                    body = resultExp
                  )
              }


            val imports =
              (context.unit.stats.collect {
                case imp: Import => imp
              } ++ methodCtxt.extraImports).distinct.sortBy(_.toString)

            (context.copy(
              companionDefinitions = context.companionDefinitions :+ methodToAdd,
              resolver = methodCtxt.resolver,
              unit = Source(imports ++ context.unit.stats.filter(!_.isInstanceOf[Import]))
              ), ())
          }
        }

      implicit val canResolveExpressionImportInCompilationUnit: Understands[CompilationUnitCtxt, ResolveImport[Import, Expression]] =
        new Understands[CompilationUnitCtxt, ResolveImport[Import, Expression]] {
          def perform(
            context: CompilationUnitCtxt,
            command: ResolveImport[Import, Term]
          ): (CompilationUnitCtxt, Option[Import]) = {
            val stripped = AnyParadigm.stripGenerics(command.forElem)
            Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
              (context, AnyParadigm.guessImport(base.config.targetPackage.ref, stripped))
            }
          }
        }
      implicit val canResolveTypeImportInCompilationUnit: Understands[CompilationUnitCtxt, ResolveImport[Import, Type]] =
        new Understands[CompilationUnitCtxt, ResolveImport[Import, Type]] {
          def perform(
            context: CompilationUnitCtxt,
            command: ResolveImport[Import, Type]
          ): (CompilationUnitCtxt, Option[Import]) = {
            val stripped = AnyParadigm.stripGenerics(command.forElem)
            Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
              (context, AnyParadigm.guessImport(base.config.targetPackage.ref, stripped))
            }
          }
        }
  }

  object typeCapabilities extends TypeCapabilities {
    implicit val canAddTypeConstructorInType: Understands[TypeCtxt, AddTypeConstructor[Syntax.MangledName, Type]] =
      new Understands[TypeCtxt, AddTypeConstructor[Syntax.MangledName, Type]] {
        def perform(
          context: TypeCtxt,
          command: AddTypeConstructor[Syntax.MangledName, Type]
        ): (TypeCtxt, Unit) = {

          def newCase(parentName: scala.meta.Type.Name): scala.meta.Defn.Class = {
            val prior = context.tpe(parentName)
            val newCase =
              Defn.Class(
                mods = List.empty,
                name = Type.Name(command.name.toAST.value),
                tparams = List.empty,
                ctor = Ctor.Primary(
                  mods = List.empty,
                  name = Name.Anonymous(),
                  paramss = List(command.parameters.toList.map { case (name, tpe) =>
                    Term.Param(
                      mods = List.empty,
                      name = name.toAST,
                      decltpe = Some(tpe),
                      default = None
                    )
                  })
                ), templ = Template(Nil, List.empty, Self(Name.Anonymous(), None), Nil)  // trying HEINEMAN
              )
            prior.copy(templ = prior.templ.copy(
              stats = prior.templ.stats :+ newCase
            ))
          }
          (context.copy(tpe = newCase), Unit)
        }
      }
    implicit val canTranslateTypeInType: Understands[TypeCtxt, ToTargetLanguageType[Type]] =
      new Understands[TypeCtxt, ToTargetLanguageType[Type]] {
        def perform(context: TypeCtxt, command: ToTargetLanguageType[Type]): (TypeCtxt, Type) =
          Command.runGenerator(context.resolver.typeTypeResolution(command.tpe), context)
      }
    implicit val canAddImportInType: Understands[TypeCtxt, AddImport[Import]] =
      new Understands[TypeCtxt, AddImport[Import]] {
        def perform(context: TypeCtxt, command: AddImport[Import]): (TypeCtxt, Unit) = {
          (context.copy(extraImports = (context.extraImports :+ command.imp).groupBy(_.structure).mapValues(_.head).values.toList), ())
        }
      }
    implicit val canResolveTypeImportInType: Understands[TypeCtxt, ResolveImport[Import, Type]] =
      new Understands[TypeCtxt, ResolveImport[Import, Type]] {
        def perform(
          context: TypeCtxt,
          command: ResolveImport[Import, Type]
        ): (TypeCtxt, Option[Import]) = {
          val stripped = AnyParadigm.stripGenerics(command.forElem)
          Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
            (context, AnyParadigm.guessImport(base.config.targetPackage.ref, stripped))
          }
        }
      }
    implicit val canResolveExpressionImportInType: Understands[TypeCtxt, ResolveImport[Import, Term]] =
      new Understands[TypeCtxt, ResolveImport[Import, Expression]] {
        def perform(
          context: TypeCtxt,
          command: ResolveImport[Import, Term]
        ): (TypeCtxt, Option[Import]) = {
          val stripped = AnyParadigm.stripGenerics(command.forElem)
          Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
            (context, AnyParadigm.guessImport(base.config.targetPackage.ref, stripped))
          }
        }
      }
    implicit val canFindTypeInType: Understands[TypeCtxt, FindType[Syntax.MangledName, Type]] =
      new Understands[TypeCtxt, FindType[Syntax.MangledName, Type]] {
        def perform(context: TypeCtxt, command: FindType[Syntax.MangledName, Type]): (TypeCtxt, Type) =
          (context, AnyParadigm.toTypeSelection(command.name))
      }
  }
  object methodBodyCapabilities extends MethodBodyCapabilities {
    implicit val canInstantiateTypeInMethod: Understands[MethodBodyCtxt, InstantiateType[Type, Syntax.MangledName, Term]] =
      new Understands[MethodBodyCtxt, InstantiateType[Type, Syntax.MangledName, Term]] {
        def perform(
          context: MethodBodyCtxt,
          command: InstantiateType[Type, Syntax.MangledName, Term]
        ): (MethodBodyCtxt, Term) = {
          (context, Term.Apply(AnyParadigm.selectConstructor(command.tpe, command.constructor.toAST).get, command.arguments.toList))
        }
      }
    implicit val canResolveExpressionImportInMethod: Understands[MethodBodyCtxt, ResolveImport[Import, Term]] =
      new Understands[MethodBodyCtxt, ResolveImport[Import, Expression]] {
        def perform(
          context: MethodBodyCtxt,
          command: ResolveImport[Import, Term]
        ): (MethodBodyCtxt, Option[Import]) = {
          val stripped = AnyParadigm.stripGenerics(command.forElem)
          Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
            (context, AnyParadigm.guessImport(base.config.targetPackage.ref, stripped))
          }
        }
      }
    implicit val canFindMethodInMethod: Understands[MethodBodyCtxt, FindMethod[Syntax.MangledName, Term]] =
      new Understands[MethodBodyCtxt, FindMethod[Syntax.MangledName, Term]] {
        def perform(context: MethodBodyCtxt, command: FindMethod[Syntax.MangledName, Term]): (MethodBodyCtxt, Term) =
          (context, AnyParadigm.toTermSelection(command.name))
      }
    implicit val canFindTypeInMethod: Understands[MethodBodyCtxt, FindType[Syntax.MangledName, Type]] =
      new Understands[MethodBodyCtxt, FindType[Syntax.MangledName, Type]] {
        def perform(context: MethodBodyCtxt, command: FindType[Syntax.MangledName, Type]): (MethodBodyCtxt, Type) =
          (context, AnyParadigm.toTypeSelection(command.name))
      }
  }
  object projectCapabilities extends ProjectCapabilities {
    implicit val canAddTypeLookupForTypesInProject: Understands[ProjectCtxt, AddTypeLookup[TypeCtxt, Type]] =
      new Understands[ProjectCtxt, AddTypeLookup[TypeCtxt, Type]] {
        def perform(context: ProjectCtxt, command: AddTypeLookup[TypeCtxt, Type]): (ProjectCtxt, Unit) = {
          def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[TypeCtxt, Type] =
            if (tpe == command.tpe) {
              command.lookup
            } else {
              context.resolver._typeTypeResolution(k)(tpe)
            }
          (context.copy(resolver = context.resolver.copy(_typeTypeResolution = newLookup)), ())
        }
      }
  }
}

object Functional {
  def apply[AP <: AnyParadigm](base: AnyParadigm): Functional[base.type] = {
    val b: base.type = base
    new Functional[b.type] {
      val base: b.type = b
    }
  }
}