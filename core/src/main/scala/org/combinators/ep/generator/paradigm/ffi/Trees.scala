package org.combinators.ep.generator.paradigm.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.ffi.FFI
import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.Understands

case class CreateLeaf[Type](valueType: Type)
case class CreateNode()

trait Trees[Context] extends FFI {
  import base.*
  import syntax.*

  trait TreeCapabilities {
    implicit val canCreateLeaf: Understands[Context, Apply[CreateLeaf[Type], Expression, Expression]]
    def createLeaf(elemTpe: Type, value: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[CreateLeaf[Type], Expression, Expression](CreateLeaf(elemTpe), Seq(value)))

    implicit val canCreateNode: Understands[Context, Apply[CreateNode, Expression, Expression]]
    def createNode(label: Expression, children: Seq[Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[CreateNode, Expression, Expression](CreateNode(), label +: children))
  }

  val treeCapabilities: TreeCapabilities
}

object Trees {
  type WithBase[Ctxt, B <: AnyParadigm] = Trees[Ctxt] { val base: B }
}

