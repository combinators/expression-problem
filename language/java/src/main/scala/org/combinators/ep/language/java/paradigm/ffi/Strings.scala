package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.expr.{BinaryExpr, MethodCallExpr, StringLiteralExpr, CharLiteralExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{Apply, GetMember}
import org.combinators.ep.generator.paradigm.ffi.{Strings => Strs, _}
import org.combinators.ep.language.java.{CodeGenerator, ContextSpecificResolver, JavaNameProvider, ProjectCtxt, Syntax}
import Syntax.default._
import org.combinators.ep.language.java.paradigm.{AnyParadigm, ObjectOriented}
import CodeGenerator.Enable
import com.github.javaparser.ast.NodeList

class Strings[Ctxt, AP <: AnyParadigm](
  val base: AP,
  getMember: Understands[Ctxt, GetMember[Expression, Name]],
  applyMethod: Understands[Ctxt, Apply[Expression, Expression, Expression]]
) extends Strs[Ctxt] {
  case object StringsEnabled

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
      implicit val canGetCharAt: Understands[Ctxt, Apply[GetCharAt, Expression, Expression]] =
        new Understands[Ctxt, Apply[GetCharAt, Expression, Expression]] {
          def perform(
                       context: Ctxt,
                       command: Apply[GetCharAt, Expression, Expression]
                     ): (Ctxt, Expression) = {
            implicit val _getMember = getMember
            implicit val _applyMethod = applyMethod
            val gen = for {
              charAtMethod <- GetMember[Expression, Name](command.arguments(0), JavaNameProvider.mangle("charAt")).interpret
              res <- Apply[Expression, Expression, Expression](charAtMethod, Seq(command.arguments(1))).interpret
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
            val gen = Command.lift[Ctxt, Expression](new MethodCallExpr(ObjectOriented.nameToExpression(ObjectOriented.fromComponents("String")), "valueOf", new NodeList[Expression](command.arguments(0))))
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
        if (!context.resolver.resolverInfo.contains(StringsEnabled)) {
          // heineman: until we have a top-level FFI Character, then sneak in type for Character here.
          val resolverUpdate =
            ContextSpecificResolver.updateResolver(base.config, TypeRep.String, ObjectOriented.nameToType(ObjectOriented.fromComponents("String")))(new StringLiteralExpr(_))
          val resolverCharUpdate =
            ContextSpecificResolver.updateResolver(base.config, TypeRep.Char, ObjectOriented.nameToType(ObjectOriented.fromComponents("Character")))(new CharLiteralExpr(_))
          (context.copy(resolver = resolverCharUpdate(resolverUpdate(context.resolver)).addInfo(StringsEnabled)), ())
        } else (context, ())
      }
    })
}