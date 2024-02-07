package org.combinators.ep.language.inbetween.ffi  /*DI:LI:AI*/

// cannot find 'lists'
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.language.inbetween.{any, polymorphism}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Append, Cons, Create, Head, Tail, Lists => Lsts}
trait Lists[FT <: ListOps.FinalTypes, FactoryType <: ListOps.Factory[FT]] extends Lsts[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory

  override val listCapabilities: ListCapabilities = new ListCapabilities {
    override implicit val canCreate: Understands[any.Method[FT], Apply[Create[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Create[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Create[any.Type[FT]], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.createList(command.functional.elementType, command.arguments))
        }
      }
    override implicit val canCons: Understands[any.Method[FT], Apply[Cons, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Cons, any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Cons, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.consList(command.arguments(0), command.arguments(1)))
        }
      }
    override implicit val canHead: Understands[any.Method[FT], Apply[Head, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Head, any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Head, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.head(command.arguments(0)))
        }
      }
    override implicit val canTail: Understands[any.Method[FT], Apply[Tail, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Tail, any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Tail, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.tail(command.arguments(0)))
        }
      }
    override implicit val canAppend: Understands[any.Method[FT], Apply[Append, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Append, any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Append, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.appendList(command.arguments(0), command.arguments(1)))
        }
      }
  }

  override def enable(): Generator[any.Project[FT], Unit] = Command.skip[any.Project[FT]]
}

object Lists {
  type WithBase[FT <: ListOps.FinalTypes, FactoryType <: ListOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Lists[FT, FactoryType] { val base: B }
  def apply[FT <: ListOps.FinalTypes, FactoryType <: ListOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Lists[FT, FactoryType] {
    val base: _base.type = _base
  }
}

object ListOps {

  trait FinalTypes extends OperatorExpressionOps.FinalTypes with polymorphism.FinalTypes {

  }
  trait CreateList[FT <: FinalTypes] extends any.Type[FT]
  trait ConsListOp[FT <: FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait HeadListOp[FT <: FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait TailListOp[FT <: FinalTypes] extends OperatorExpressionOps.Operator[FT]
  trait AppendListOp[FT <: FinalTypes] extends OperatorExpressionOps.Operator[FT]

  trait Factory[FT <: FinalTypes] extends OperatorExpressionOps.Factory[FT] with polymorphism.Factory[FT] {
    def createList(): CreateList[FT]

    def createList(tpe: any.Type[FT], elems: Seq[any.Expression[FT]]): any.ApplyExpression[FT] =
      applyExpression(typeReferenceExpression(typeApplication(createList(), Seq(tpe))), elems)

    def consListOp(): ConsListOp[FT]
    def headListOp(): HeadListOp[FT]

    def tailListOp(): TailListOp[FT]

    def appendListOp(): AppendListOp[FT]

    def consList(element: any.Expression[FT], list: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(consListOp(), element, list)
    def head(list: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(headListOp(), list)

    def tail(list: any.Expression[FT]): OperatorExpressionOps.UnaryExpression[FT] =
      unaryExpression(tailListOp(), list)
    def appendList(left: any.Expression[FT], right: any.Expression[FT]): OperatorExpressionOps.BinaryExpression[FT] =
      binaryExpression(appendListOp(), left, right)
  }
}
