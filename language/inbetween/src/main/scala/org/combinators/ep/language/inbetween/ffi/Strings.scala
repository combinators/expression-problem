package org.combinators.ep.language.inbetween.ffi   /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{GetStringLength, StringAppend, ToString, Strings as Strs}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

// cannot find 'strings'
trait Strings[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: StringOps.Factory[FT]] extends Strs[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory

  val stringCapabilities: StringCapabilities = new StringCapabilities {
    implicit val canGetStringLength: Understands[any.Method[FT], Apply[GetStringLength, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[GetStringLength, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[GetStringLength, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.stringLength(command.arguments.head))
        }
      }

    implicit val canAppend: Understands[any.Method[FT], Apply[StringAppend, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[StringAppend, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[StringAppend, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, command.arguments.tail.foldLeft(command.arguments.head){case (r, l) => factory.appendString(r, l)})
        }
      }
    implicit val canToStringInCtxt: Understands[any.Method[FT], Apply[ToString[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[ToString[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[ToString[any.Type[FT]], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.toString(command.arguments.head))
        }
      }
  }
  def enable(): Generator[any.Project[FT], Unit] = Command.skip[any.Project[FT]]
}

object Strings {
  type WithBase[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: StringOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Strings[FT, FactoryType] { val base: B }
  def apply[FT <: OperatorExpressionOps.FinalTypes, FactoryType <: StringOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Strings[FT, FactoryType] {
    val base: _base.type = _base
  }
}

object StringOps {
  trait ToStringOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait AppendStringOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait StringLengthOp[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Operator[FT]


  trait Factory[FT <: OperatorExpressionOps.FinalTypes] extends OperatorExpressionOps.Factory[FT] {
    def toStringOp(): ToStringOp[FT]
    def appendStringOp(): AppendStringOp[FT]
    def stringLengthOp(): StringLengthOp[FT]

    def toString(exp: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(toStringOp(), exp)
    def appendString(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(appendStringOp(), left, right)
    def stringLength(exp: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(stringLengthOp(), exp)
  }
}