package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.expr.{BinaryExpr, BooleanLiteralExpr, MethodCallExpr, TypeExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{Apply, GetMember}
import org.combinators.ep.generator.paradigm.ffi.{Equals, Equality => Eql}
import org.combinators.ep.language.java.{ContextSpecificResolver, JavaNameProvider, ProjectCtxt}
import org.combinators.ep.language.java.paradigm.{AnyParadigm, ffi}
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.CodeGenerator.Enable

class Equality[Ctxt, AP <: AnyParadigm](
  val base: AP,
  getMember: Understands[Ctxt, GetMember[Expression, Name]],
  applyMethod: Understands[Ctxt, Apply[Expression, Expression, Expression]]
) extends Eql[Ctxt] {
  case object EqualityEnabled

  val equalityCapabilities: EqualityCapabilities =
    new EqualityCapabilities {
      implicit val canEquals: Understands[Ctxt, Apply[Equals[Type], Expression, Expression]] =
        new Understands[Ctxt, Apply[Equals[Type], Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[Equals[Type], Expression, Expression]
          ): (Ctxt, Expression) = {
            val tpe = command.functional.inType.toClassOrInterfaceType
            if (tpe.isPresent) {
              implicit val _getMember = getMember
              implicit val _applyMethod = applyMethod
              val boxedLhs =
                if (!base.config.boxLevel.isConsistent && tpe.get.isBoxedType) {
                  new MethodCallExpr(new TypeExpr(tpe.get()), "valueOf", new NodeList[Expression](command.arguments.head))
                } else {
                  command.arguments.head
                }
              val gen = for {
                equalsMethod <- GetMember[Expression, Name](boxedLhs, JavaNameProvider.mangle("equals", Set.empty)).interpret
                res <- Apply[Expression, Expression, Expression](equalsMethod, command.arguments.tail).interpret
              } yield res
              Command.runGenerator(gen, context)
            } else {
              (context, new BinaryExpr(command.arguments(0), command.arguments(1), BinaryExpr.Operator.EQUALS))
            }
          }
        }
    }
  def enable(): Generator[base.ProjectContext, Unit] = Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
    def perform(
      context: ProjectCtxt,
      command: Enable.type
    ): (ProjectCtxt, Unit) = {
      if (!context.resolver.resolverInfo.contains(EqualityEnabled)) {
        val resolverUpdate =
          ContextSpecificResolver.updateResolver(base.config, TypeRep.Boolean, PrimitiveType.booleanType())(new BooleanLiteralExpr(_))
        (context.copy(resolver = resolverUpdate(context.resolver).addInfo(EqualityEnabled)), ())
      } else (context, ())
    }
  })
}
