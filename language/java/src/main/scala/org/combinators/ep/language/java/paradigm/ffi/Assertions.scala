package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.expr.{BooleanLiteralExpr, NameExpr}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Assert, Assertions => Assrts}
import org.combinators.ep.language.java.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.language.java.{ContextSpecificResolver, JavaNameProvider, MethodBodyCtxt, ProjectCtxt}
import org.combinators.ep.language.java.Syntax.default._
import org.combinators.ep.language.java.CodeGenerator.Enable

class Assertions[AP <: AnyParadigm](val base: AP)(ooParadigm: ObjectOriented[AP]) extends Assrts[MethodBodyCtxt] {
  val assertImp = new ImportDeclaration("org.junit.Assert", false, false)

  case object AssertionsEnabled

  val assertionCapabilities: AssertionCapabilities =
    new AssertionCapabilities {
      implicit val canAssert: Understands[MethodBodyCtxt, Apply[Assert, Expression, Expression]] =
        new Understands[MethodBodyCtxt, Apply[Assert, Expression, Expression]] {
          def perform(
            context: MethodBodyCtxt,
            command: Apply[Assert, Expression, Expression]
          ): (MethodBodyCtxt, Expression) = {
            import ooParadigm.methodBodyCapabilities._
            import base.methodBodyCapabilities._
            val assertCls = ObjectOriented.nameToExpression(assertImp.getName)
            val gen = for {
              _ <- addImport(assertImp)
              assertMethod <- getMember(assertCls, JavaNameProvider.mangle("assertTrue"))
              msg <- reify[String](TypeRep.String, command.functional.message)
              res <- apply(assertMethod, Seq(msg, command.arguments(0)))
            } yield res
            Command.runGenerator(gen, context)
          }
        }
    }

  override def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        if (!context.resolver.resolverInfo.contains(AssertionsEnabled)) {
          val assertTpe = ObjectOriented.nameToType(assertImp.getName)
          val resolverUpdate =
            ContextSpecificResolver
              .updateResolver(base.config, TypeRep.Boolean, PrimitiveType.booleanType())(new BooleanLiteralExpr(_))
              .andThen(resolver =>
                resolver.copy(
                  _importResolution = k => {
                    case tpe if tpe == assertTpe => Some(assertImp)
                    case other => context.resolver._importResolution(k)(other)
                  }
                ).addInfo(AssertionsEnabled)
              )

          (context.copy(resolver = resolverUpdate(context.resolver)), ())
        } else (context, ())
      }
    })
}
