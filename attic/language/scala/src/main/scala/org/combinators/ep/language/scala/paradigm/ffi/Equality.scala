package org.combinators.ep.language.scala.paradigm.ffi   /*DI:LD:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{Apply, GetMember}
import org.combinators.ep.generator.paradigm.ffi.{Equals, Equality => Eql}
import org.combinators.ep.language.scala.CodeGenerator.Enable
import org.combinators.ep.language.scala.OperatorExprs.infixExprOp
import org.combinators.ep.language.scala.{ContextSpecificResolver, ProjectCtxt}
import org.combinators.ep.language.scala.paradigm.AnyParadigm

import scala.meta._

class Equality[Ctxt, AP <: AnyParadigm](val base: AP) extends Eql[Ctxt] {
  import base.syntax._
  case object EqualityEnabled
  
  val equalityCapabilities: EqualityCapabilities =
    new EqualityCapabilities {
      implicit val canEquals: Understands[Ctxt, Apply[Equals[Type], Expression, Expression]] =
        infixExprOp(Term.Name("=="))
    }
  def enable(): Generator[base.ProjectContext, Unit] = Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
    def perform(
      context: ProjectCtxt,
      command: Enable.type
    ): (ProjectCtxt, Unit) = {
      if (!context.resolver.resolverInfo.contains(EqualityEnabled)) {
        val resolverUpdate =
          ContextSpecificResolver.updateResolver(base.config, TypeRep.Boolean, Type.Name("Boolean"))(Lit.Boolean(_))
        (context.copy(resolver = resolverUpdate(context.resolver)), ())
      } else (context, ())
    }
  })
}