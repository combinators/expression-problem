package org.combinators.ep.language.scala.paradigm.ffi

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{Apply, Functional}
import org.combinators.ep.generator.paradigm.ffi.{Assert, Assertions => Assrts}
import org.combinators.ep.language.scala.CodeGenerator.Enable
import org.combinators.ep.language.scala.{ContextSpecificResolver, MethodBodyCtxt, ProjectCtxt}
import org.combinators.ep.language.scala.Syntax.MangledName
import org.combinators.ep.language.scala.paradigm.AnyParadigm

import scala.meta._


class Assertions[AP <: AnyParadigm](val base: AP)(functional: Functional.WithBase[AP]) extends Assrts[MethodBodyCtxt] {
  import base.syntax._

  val assertionCapabilities: AssertionCapabilities =
    new AssertionCapabilities {
      implicit val canAssert: Understands[MethodBodyCtxt, Apply[Assert, Expression, Expression]] =
        new Understands[MethodBodyCtxt, Apply[Assert, Expression, Expression]] {
          def perform(
            context: MethodBodyCtxt,
            command: Apply[Assert, Expression, Expression]
          ): (MethodBodyCtxt, Expression) = {
            import functional.methodBodyCapabilities._
            import base.methodBodyCapabilities._
            val gen = for {
              assertMethod <- findMethod(Seq(MangledName.fromAST(Term.Name("assert"))))
              msg <- reify[String](TypeRep.String, command.functional.message)
              res <- apply(assertMethod,
                if (command.functional.message.isEmpty) Seq(command.arguments.head)
                else Seq(command.arguments.head, msg))
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

        val resolverUpdate =
          ContextSpecificResolver
            .updateResolver(base.config, TypeRep.Boolean, Type.Name("Boolean"))(Lit.Boolean(_))

        (context.copy(resolver = resolverUpdate(context.resolver)), ())
      }
    })
}