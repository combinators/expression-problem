package org.combinators.ep.language.scala.paradigm.ffi   /*DI:LD:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{Apply, GetMember}
import org.combinators.ep.generator.paradigm.ffi.{GetStringLength, StringAppend, ToString, Strings => Strs}
import org.combinators.ep.language.scala.CodeGenerator.Enable
import org.combinators.ep.language.scala.{ContextSpecificResolver, ProjectCtxt}
import org.combinators.ep.language.scala.paradigm.AnyParadigm

import scala.meta._

class Strings[Ctxt, AP <: AnyParadigm](val base: AP) extends Strs[Ctxt] {
  import base.syntax._
  case object StringsEnabled

  val stringCapabilities: StringCapabilities =
    new StringCapabilities {
      implicit val canGetStringLength: Understands[Ctxt, Apply[GetStringLength, Expression, Expression]] =
        new Understands[Ctxt, Apply[GetStringLength, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[GetStringLength, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, Term.Select(command.arguments.head, Term.Name("length")))
          }
        }
      implicit val canAppend: Understands[Ctxt, Apply[StringAppend, Expression, Expression]] =
        new Understands[Ctxt, Apply[StringAppend, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[StringAppend, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, command.arguments.tail.foldLeft(command.arguments.head){ case (str, next) =>
              Term.ApplyInfix(
                lhs = str,
                op = Term.Name("++"),
                targs = List.empty,
                args = List(next)
              )
            })
          }
        }
      implicit val canToStringInCtxt: Understands[Ctxt, Apply[ToString[Type], Expression, Expression]] =
        new Understands[Ctxt, Apply[ToString[Type], Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[ToString[Type], Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, Term.Apply(Term.Select(Term.Name("String"), Term.Name("valueOf")), command.arguments.toList))
          }
        }
    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        if (!context.resolver.resolverInfo.contains(StringsEnabled)) {
          val resolverUpdate =
            ContextSpecificResolver.updateResolver(base.config, TypeRep.String, Type.Name("String"))(Lit.String(_))
          (context.copy(resolver = resolverUpdate(context.resolver)), ())
        } else (context, ())
      }
    })
}