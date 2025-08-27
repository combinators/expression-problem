package org.combinators.ep.builder.inbetween.paradigm.ffi

/*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax
import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{CreateLeaf, CreateNode, Trees as Trs}
import org.combinators.cogen.{Command, TypeRep, Understands}
import org.combinators.ep.domain.abstractions.DomainTpeRep
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.ffi.{OperatorExpressionOps, TreeOps, Trees}
import org.combinators.ep.language.inbetween.{any, polymorphism}

// cannot find 'trees'
trait Trees[FT <: TreeOps.FinalTypes, Ctx <: any.Method[FT], FactoryType <: TreeOps.Factory[FT]] extends Trs[Ctx] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory
  val treeLibrary: Map[Seq[any.Name[FT]], Generator[any.CompilationUnit[FT], Unit]]
  def addCtxTypeLookup(tpe: TypeRep, lookup: any.Type[FT]): Generator[any.Project[FT], Unit]

  override val treeCapabilities: TreeCapabilities = new TreeCapabilities {
    implicit val canCreateLeaf: Understands[Ctx, Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] =
      new Understands[Ctx, Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: Ctx, command: Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]): (Ctx, any.Expression[FT]) = {
          (context, factory.createLeaf(command.functional.valueType, command.arguments.head))
        }
      }
    implicit val canCreateNode: Understands[Ctx, Apply[CreateNode, any.Expression[FT], any.Expression[FT]]] =
      new Understands[Ctx, Apply[CreateNode, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: Ctx, command: Apply[CreateNode, any.Expression[FT], any.Expression[FT]]): (Ctx, any.Expression[FT]) = {
          (context, factory.createNode(command.arguments.head, command.arguments.tail))
        }
      }
  }

  override def enable(): Generator[any.Project[FT], Unit] = {
    import base.projectCapabilities.*
    import syntax.forEach
    for {
      _ <- forEach(treeLibrary.toSeq){ case (qualifiedName, compilationUnit) =>
        addCompilationUnit(compilationUnit, qualifiedName*)
      }
      nodeTpe <- Command.lift(factory.nodeTpe())
      _ <- addCtxTypeLookup(DomainTpeRep.Tree, nodeTpe)
    } yield ()
  }
}

object Trees {
  type WithBase[FT <: TreeOps.FinalTypes, Ctx <: any.Method[FT], FactoryType <: TreeOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Trees[FT, Ctx, FactoryType] { val base: B }
  def apply[FT <: TreeOps.FinalTypes, Ctx <: any.Method[FT], FactoryType <: TreeOps.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](
     _base: B)(
     _treeLibrary: Map[Seq[any.Name[FT]], Generator[any.CompilationUnit[FT], Unit]],
    _addCtxTypeLookup: (tpe: TypeRep, lookup: any.Type[FT]) => Generator[any.Project[FT], Unit]
   ): WithBase[FT, Ctx, FactoryType, _base.type] = new Trees[FT, Ctx, FactoryType] {
    val base: _base.type = _base
    val treeLibrary = _treeLibrary
    override def addCtxTypeLookup(tpe: TypeRep, lookup: any.Type[FT]): Generator[any.Project[FT], Unit] = _addCtxTypeLookup(tpe, lookup)
  }
}

object TreeOps {
  trait FinalTypes extends OperatorExpressionOps.FinalTypes with polymorphism.FinalTypes {
    type NodeTpe <: Type
    type LeafTpe <: Type
    type CreateNodeExpr <: Expression
  }
  
  trait NodeTpe[FT <: FinalTypes] extends any.Type[FT] {
    def getSelfCreateNode: finalTypes.NodeTpe
  }
  
  trait LeafTpe[FT <: FinalTypes] extends any.Type[FT] {
    def getSelfCreateLeaf: finalTypes.LeafTpe
  }

  trait CreateNodeExpr[FT <: FinalTypes] extends any.Expression[FT] {
    def getSelfCreateNodeExpr: finalTypes.CreateNodeExpr
  }
  
  trait Factory[FT <: FinalTypes] extends OperatorExpressionOps.Factory[FT] with polymorphism.Factory[FT] {
    def createNodeExpr(): CreateNodeExpr[FT]
    def leafTpe(): LeafTpe[FT]
    def nodeTpe(): NodeTpe[FT]

    def createNode(label: any.Expression[FT], children: Seq[any.Expression[FT]]): any.ApplyExpression[FT] =
      applyExpression(createNodeExpr(), label +: children)

    def createLeaf(tpe: any.Type[FT], value: any.Expression[FT]): any.ApplyExpression[FT] =
      applyExpression(typeReferenceExpression(typeApplication(leafTpe(), Seq(tpe))), Seq(value))

    implicit def convert(other: NodeTpe[FT]): NodeTpe[FT]
    implicit def convert(other: LeafTpe[FT]): LeafTpe[FT]
    implicit def convert(other: CreateNodeExpr[FT]): CreateNodeExpr[FT]
  }
}
