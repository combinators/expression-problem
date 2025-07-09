package org.combinators.ep.language.inbetween.ffi   /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.{any, polymorphism}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.cogen.paradigm.AnyParadigm.syntax
import org.combinators.cogen.paradigm.ffi.{CreateLeaf, CreateNode, Trees as Trs}

// cannot find 'trees'
trait Trees[FT <: TreeOps.FinalTypes, FactoryType <: TreeOps.Factory[FT]] extends Trs[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory
  val treeLibrary: Map[Seq[any.Name[FT]], Generator[any.CompilationUnit[FT], Unit]]

  override val treeCapabilities: TreeCapabilities = new TreeCapabilities {
    implicit val canCreateLeaf: Understands[any.Method[FT], Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.createLeaf(command.functional.valueType, command.arguments.head))
        }
      }
    implicit val canCreateNode: Understands[any.Method[FT], Apply[CreateNode, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[CreateNode, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[CreateNode, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.createNode(command.arguments.head, command.arguments.tail))
        }
      }
  }

  override def enable(): Generator[any.Project[FT], Unit] = {
    import base.projectCapabilities._
    import syntax.forEach
    for {
      _ <- forEach(treeLibrary.toSeq){ case (qualifiedName, compilationUnit) =>
        addCompilationUnit(compilationUnit, qualifiedName*)
      }
    } yield ()
  }
}

object Trees {
  type WithBase[FT <: TreeOps.FinalTypes, FactoryType <: TreeOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Trees[FT, FactoryType] { val base: B }
  def apply[FT <: TreeOps.FinalTypes, FactoryType <: TreeOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](
     _base: B)(
     _treeLibrary: Map[Seq[any.Name[FT]], Generator[any.CompilationUnit[FT], Unit]]
   ): WithBase[FT, FactoryType, _base.type] = new Trees[FT, FactoryType] {
    val base: _base.type = _base
    val treeLibrary = _treeLibrary
  }
}

object TreeOps {

  trait FinalTypes extends OperatorExpressionOps.FinalTypes with polymorphism.FinalTypes {
    type CreateLeaf <: Type
    type CreateNodeExpr <: Expression
  }
  trait CreateLeaf[FT <: FinalTypes] extends any.Type[FT] {
    def getSelfCreateLeaf: finalTypes.CreateLeaf
  }

  trait CreateNodeExpr[FT <: FinalTypes] extends any.Expression[FT] {
    def getSelfCreateNodeExpr: finalTypes.CreateNodeExpr
  }
  trait Factory[FT <: FinalTypes] extends OperatorExpressionOps.Factory[FT] with polymorphism.Factory[FT] {
    def createNodeExpr(): CreateNodeExpr[FT]
    def createLeaf(): CreateLeaf[FT]

    def createNode(label: any.Expression[FT], children: Seq[any.Expression[FT]]): any.ApplyExpression[FT] =
      applyExpression(createNodeExpr(), label +: children)

    def createLeaf(tpe: any.Type[FT], value: any.Expression[FT]): any.ApplyExpression[FT] =
      applyExpression(typeReferenceExpression(typeApplication(createLeaf(), Seq(tpe))), Seq(value))

    implicit def convert(other: CreateLeaf[FT]): CreateLeaf[FT]
    implicit def convert(other: CreateNodeExpr[FT]): CreateNodeExpr[FT]
  }
}
