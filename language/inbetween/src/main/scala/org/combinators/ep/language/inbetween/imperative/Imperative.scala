package org.combinators.ep.language.inbetween.imperative

import org.combinators.ep.generator
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control
import org.combinators.ep.generator.{Command, FileWithPath, Understands, paradigm}
import org.combinators.ep.generator.paradigm.control.{Return, Imperative => Imp}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm


// Requires "recursive solution" to the EP, where Ctxt has a producer method and so this needs an EP solution, while
// talking about something which doesn't need to have one..
trait Imperative[FT <: FinalTypes, FactoryType <: Factory[FT], Ctxt] extends Imp[Ctxt] {
  val base: AnyParadigm[FT, FactoryType]
  import base.factory

  def pushToContext(context: Ctxt, stmt: any.Statement[FT]): Ctxt

  val imperativeCapabilities: ImperativeCapabilities = new ImperativeCapabilities {
    implicit val canDeclareVariable: Understands[Ctxt, paradigm.DeclareVariable[any.Name[FT], any.Type[FT], Option[any.Expression[FT]], any.Expression[FT]]] = new Understands[Ctxt, paradigm.DeclareVariable[any.Name[FT], any.Type[FT], Option[any.Expression[FT]], any.Expression[FT]]] {
      def perform(context: Ctxt, command: paradigm.DeclareVariable[any.Name[FT], any.Type[FT], Option[any.Expression[FT]], any.Expression[FT]]): (Ctxt, any.Expression[FT]) = {
        (pushToContext(context, factory.declareVariable(command.name, command.tpe, command.initialization)), factory.variableReferenceExpression(command.name))
      }
    }

    implicit val canAssignVariable: Understands[Ctxt, control.AssignVariable[any.Expression[FT], any.Statement[FT]]] = ???
    implicit val canLiftExpression: Understands[Ctxt, control.LiftExpression[any.Expression[FT], any.Statement[FT]]] = ???
    implicit val canIfThenElse: Understands[Ctxt, paradigm.IfThenElse[any.Expression[FT], Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], any.Statement[FT]]] = ???
    implicit val canWhile: Understands[Ctxt, control.While[Ctxt, any.Expression[FT], any.Statement[FT]]] = ???
    implicit val canReturn: Understands[Ctxt, Return[any.Expression[FT], any.Statement[FT]]] = ???
  }

}