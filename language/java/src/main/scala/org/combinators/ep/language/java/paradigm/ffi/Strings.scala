package org.combinators.ep.language.java.paradigm.ffi

import com.github.javaparser.ast.expr.{BinaryExpr, StringLiteralExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{Apply, GetMember}
import org.combinators.ep.generator.paradigm.ffi.{Strings => Strs, _}
import org.combinators.ep.language.java.{CodeGenerator, ContextSpecificResolver, JavaNameProvider, ProjectCtxt, Syntax}
import org.combinators.templating.twirl.Java
import Syntax.default._
import org.combinators.ep.language.java.paradigm.AnyParadigm
import CodeGenerator.Enable

class Strings[Ctxt, AP <: AnyParadigm](
  val base: AP,
  getMember: Understands[Ctxt, GetMember[Expression, Name]],
  applyMethod: Understands[Ctxt, Apply[Expression, Expression, Expression]]
) extends Strs[Ctxt] {
  val stringCapabilities: StringCapabilities =
    new StringCapabilities {
      implicit val canGetStringLength: Understands[Ctxt, Apply[GetStringLength, Expression, Expression]] =
        new Understands[Ctxt, Apply[GetStringLength, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[GetStringLength, Expression, Expression]
          ): (Ctxt, Expression) = {
            implicit val _getMember = getMember
            implicit val _applyMethod = applyMethod
            val gen = for {
              lengthMethod <- GetMember[Expression, Name](command.arguments(0), JavaNameProvider.mangle("length")).interpret
              res <- Apply[Expression, Expression, Expression](lengthMethod, Seq.empty).interpret
            } yield res
            Command.runGenerator(gen, context)
          }
        }
      implicit val canAppend: Understands[Ctxt, Apply[StringAppend, Expression, Expression]] =
        new Understands[Ctxt, Apply[StringAppend, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[StringAppend, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, command.arguments.tail.foldLeft(command.arguments.head){ case (str, next) =>
              new BinaryExpr(str, next, BinaryExpr.Operator.PLUS)
            })
          }
        }
      implicit val canToStringInCtxt: Understands[Ctxt, Apply[ToString[Type], Expression, Expression]] =
        new Understands[Ctxt, Apply[ToString[Type], Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[ToString[Type], Expression, Expression]
          ): (Ctxt, Expression) = {
            implicit val _getMember = getMember
            implicit val _applyMethod = applyMethod
            val gen = Command.lift[Ctxt, Expression](Java(s"String.valueOf(${command.arguments.head})").expression())
            Command.runGenerator(gen, context)
          }
        }
    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        val resolverUpdate =
          ContextSpecificResolver.updateResolver(base.config, TypeRep.String, Java("String").tpe())(new StringLiteralExpr(_))
        (context.copy(resolver = resolverUpdate(context.resolver)), ())
      }
    })
}