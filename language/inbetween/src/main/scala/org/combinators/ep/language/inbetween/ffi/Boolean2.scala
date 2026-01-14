package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{And, False, Not, Or, True, Booleans as Bools}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.Understands
import org.combinators.cogen.paradigm.ffi.*
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}
import org.combinators.cogen.Command

// cannot find 'boolean'
trait Boolean2[AST <: BooleanAST, B](val _base: AnyParadigm2.WithAST[AST] & B) {
  trait BooleansInMethods extends Bools[_base.ast.any.Method] {
    override val base: _base.type = _base

    import base.ast.any
    import base.ast.booleanOpsFactory

    val booleanCapabilities: BooleanCapabilities =
      new BooleanCapabilities {
        implicit val canAnd: Understands[any.Method, Apply[And, any.Expression, any.Expression]] = new Understands[any.Method, Apply[And, any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[And, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            val andExp = if (command.arguments.isEmpty) {
              booleanOpsFactory.falseExp()
            } else {
              command.arguments.reverse.tail.foldRight(command.arguments.reverse.head) { case (s, arg) => booleanOpsFactory.and(arg, s) }
            }
            (context, andExp)
          }
        }
        implicit val canOr: Understands[any.Method, Apply[Or, any.Expression, any.Expression]] = new Understands[any.Method, Apply[Or, any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Or, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            val orExp = if (command.arguments.isEmpty) {
              booleanOpsFactory.trueExp()
            } else {
              command.arguments.reverse.tail.foldRight(command.arguments.reverse.head) { case (s, arg) => booleanOpsFactory.or(arg, s) }
            }
            (context, orExp)
          }
        }
        implicit val canNot: Understands[any.Method, Apply[Not, any.Expression, any.Expression]] = new Understands[any.Method, Apply[Not, any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Not, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, booleanOpsFactory.not(command.arguments.head))
          }
        }
        implicit val canTrue: Understands[any.Method, True[any.Expression]] = new Understands[any.Method, True[any.Expression]] {
          def perform(context: any.Method, command: True[any.Expression]): (any.Method, any.Expression) = {
            (context, booleanOpsFactory.trueExp())
          }
        }
        implicit val canFalse: Understands[any.Method, False[any.Expression]] = new Understands[any.Method, False[any.Expression]] {
          def perform(context: any.Method, command: False[any.Expression]): (any.Method, any.Expression) = {
            (context, booleanOpsFactory.falseExp())
          }
        }
      }
    override def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
  
  val booleansInMethodsInMethods: BooleansInMethods = new BooleansInMethods {}
}

object Boolean2 {
  type WithBase[AST <: BooleanAST, B <: AnyParadigm2.WithAST[AST]] = Boolean2[AST, B] {}

  def apply[AST <: BooleanAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, B] = new Boolean2[AST, B](_base) {}
}
