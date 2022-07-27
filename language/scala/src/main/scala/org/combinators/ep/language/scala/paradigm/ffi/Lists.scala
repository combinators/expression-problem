package org.combinators.ep.language.scala.paradigm.ffi   /*DI:LD:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{AddImport, Apply, GetMember}
import org.combinators.ep.generator.paradigm.ffi.{Lists => Lsts, _}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.language.scala.CodeGenerator.Enable
import org.combinators.ep.language.scala.{ContextSpecificResolver, OperatorExprs, ProjectCtxt}
import org.combinators.ep.language.scala.paradigm.AnyParadigm

import scala.meta.{Import, Importee, Importer, Name, Term, Type}

class Lists[Ctxt, AP <: AnyParadigm](val base: AP) extends Lsts[Ctxt] {
  import base.syntax._
  case object ListsEnabled
  
  def listCreation[Ctxt]: Understands[Ctxt, Apply[Create[Type], Expression, Expression]] =
    new Understands[Ctxt, Apply[Create[Type], Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Create[Type], Expression, Expression]
      ): (Ctxt, Expression) = {
        val listCtor =
          Term.Select(
            Term.Select(Term.Select(Term.Name("scala"), Term.Name("collection")),
              Term.Name("immutable")
            ),
            Term.Name("List")
          )

        (context, Term.Apply(Term.ApplyType(listCtor, List(command.functional.elementType)), command.arguments.toList))
      }
    }

  val listCapabilities: ListCapabilities =
    new ListCapabilities {
      implicit val canCreate: Understands[Ctxt, Apply[Create[Type], Term, Term]] =
        listCreation
      implicit val canCons: Understands[Ctxt, Apply[Cons, Term, Term]] =
        OperatorExprs.infixExprOp(Term.Name(":+"))
      implicit val canHead: Understands[Ctxt, Apply[Head, Term, Term]] =
        new Understands[Ctxt, Apply[Head, Term, Term]] {
          def perform(context: Ctxt, command: Apply[Head, Term, Term]): (Ctxt, Term) =
            (context, Term.Select(command.arguments.head, Term.Name("head")))
        }
      implicit val canTail: Understands[Ctxt, Apply[Tail, Term, Term]] =
        new Understands[Ctxt, Apply[Tail, Term, Term]] {
          def perform(context: Ctxt, command: Apply[Tail, Term, Term]): (Ctxt, Term) =
            (context, Term.Select(command.arguments.head, Term.Name("tail")))
        }
      implicit val canAppend: Understands[Ctxt, Apply[Append, Term, Term]] =
        OperatorExprs.infixExprOp(Term.Name("++"))
    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        if (!context.resolver.resolverInfo.contains(ListsEnabled)) {
          val listType =
            Type.Select(
              Term.Select(Term.Select(Term.Name("scala"), Term.Name("collection")),
                Term.Name("immutable")
              ),
              Type.Name("List")
            )

          def updateResolver(resolver: ContextSpecificResolver): ContextSpecificResolver = {
            def addResolutionType[Ctxt](
              toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type]
            ): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
              case TypeRep.Sequence(elemRep) =>
                for {
                  elemType <- projectResolution(k)(elemRep)
                } yield Type.Apply(listType, List(elemType))
              case other => toResolution(k)(other)
            }

            def addReification[Ctxt](
              reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              projectReiification: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              canCreateList: Understands[Ctxt, Apply[Create[Type], Expression, Expression]]
            ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] =
              k => rep => rep.tpe match {
                case TypeRep.Sequence(elemTypeRep) =>
                  for {
                    elems <- forEach(rep.inst.asInstanceOf[Seq[elemTypeRep.HostType]]) { elem =>
                      projectReiification(k)(InstanceRep(elemTypeRep)(elem))
                    }
                    elemType <- projectResolution(k)(elemTypeRep)
                    res <- Apply[Create[Type], Expression, Expression](Create(elemType), elems).interpret(canCreateList)
                  } yield res
                case _ => reify(k)(rep)
              }

            def addExtraImport(
              importResolution: ContextSpecificResolver => Type => Option[Import]
            ): ContextSpecificResolver => Type => Option[Import] = k => {
              case tpe
                if AnyParadigm.stripGenerics(tpe).structure == listType.structure => Some(
                Import(List(
                  Importer(
                    Term.Select(Term.Select(Term.Name("scala"), Term.Name("collection")),
                      Term.Name("immutable")
                    ),
                    List(Importee.Name(Name.Indeterminate("List")))
                  ))))
              case other => importResolution(k)(other)
            }

            resolver.copy(
              _methodTypeResolution =
                addResolutionType(
                  resolver._methodTypeResolution,
                  _.methodTypeResolution
                ),
              _constructorTypeResolution =
                addResolutionType(
                  resolver._constructorTypeResolution,
                  _.constructorTypeResolution
                ),
              _classTypeResolution =
                addResolutionType(
                  resolver._classTypeResolution,
                  _.classTypeResolution
                ),
              _typeTypeResolution =
                addResolutionType(
                  resolver._typeTypeResolution,
                  _.typeTypeResolution
                ),
              _reificationInConstructor =
                addReification(
                  resolver._reificationInConstructor,
                  _.constructorTypeResolution,
                  _.reificationInConstructor,
                  listCreation
                ),
              _reificationInMethod =
                addReification(
                  resolver._reificationInMethod,
                  _.methodTypeResolution,
                  _.reificationInMethod,
                  listCreation
                ),
              _tpeImportResolution = addExtraImport(resolver._tpeImportResolution)
            )
          }

          (context.copy(resolver = updateResolver(context.resolver)), ())
        } else (context, ())
      }
    })

}
