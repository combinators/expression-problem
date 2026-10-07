package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.`type`.{PrimitiveType, VoidType}
import com.github.javaparser.ast.expr.{BinaryExpr, BooleanLiteralExpr, NullLiteralExpr, UnaryExpr}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{TypeRep, Understands}
import org.combinators.cogen.paradigm.ffi.Units as Uns
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.paradigm.AnyParadigm
import org.combinators.ep.language.java.{ContextSpecificResolver, OperatorExprs, ProjectCtxt, Syntax}

class Units[Ctxt, AP <: AnyParadigm](val base: AP) extends Uns[Ctxt] {
  case object UnitsEnabled

  val unitCapabilities: UnitCapabilities =
    new UnitCapabilities {
     
    }
  def enable(): Generator[base.ProjectContext, Boolean] =
    Enable.interpret(using new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Boolean) = {
        if (!context.resolver.resolverInfo.contains(UnitsEnabled)) {
          val resolverUpdate =
            ContextSpecificResolver.updateResolver(base.config, TypeRep.Unit, new VoidType())(x => new NullLiteralExpr())   // Note: Inherent mismatch between Unit and Void
          (context.copy(resolver = resolverUpdate(context.resolver).addInfo(UnitsEnabled)), true)
        } else (context, false)
      }
    })
}
