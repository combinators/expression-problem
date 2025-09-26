package org.combinators.ep.builder.inbetween.paradigm.ffi

/*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax
import org.combinators.cogen.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{CreateLeaf, CreateNode, Trees as Trs}
import org.combinators.cogen.{Command, TypeRep, Understands}
import org.combinators.ep.domain.abstractions.DomainTpeRep
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.{any, polymorphism}
import org.combinators.ep.language.inbetween.ffi.OperatorExpressionOps

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
  trait FinalTypes[BFT <: OperatorExpressionOps.FinalTypes & polymorphism.FinalTypes](val finalTypes: BFT) {
    type NodeTpe <: finalTypes.Type
    type LeafTpe <: finalTypes.Type
    type CreateNodeExpr <: finalTypes.Expression
  }
  
  trait NodeTpe[C <: Config](val config: C) extends any.Type[config.baseFactory.type] {
    def getSelfCreateNode: config.finalTreeTypes.NodeTpe
  }
  
  trait LeafTpe[C <: Config](val config: C)  extends any.Type[config.baseFactory.type] {
    def getSelfCreateLeaf: config.finalTreeTypes.LeafTpe
  }

  trait CreateNodeExpr[C <: Config](val config: C)  extends any.Expression[config.baseFactory.type] {
    def getSelfCreateNodeExpr: config.finalTreeTypes.CreateNodeExpr
  }
  
  trait Factory[C <: Config] {
    val config: C
    import config.baseFactory.{applyExpression, typeReferenceExpression, typeApplication}
    def createNodeExpr(): CreateNodeExpr[config.type]
    def leafTpe(): LeafTpe[config.type]
    def nodeTpe(): NodeTpe[config.type]

    def createNode(label: any.Expression[config.baseFinalTypes.type], children: Seq[any.Expression[config.baseFinalTypes.type]]): any.ApplyExpression[config.baseFinalTypes.type] =
      config.baseFactory.applyExpression(createNodeExpr(), label +: children)

    def createLeaf(tpe: any.Type[config.baseFinalTypes.type], value: any.Expression[config.baseFinalTypes.type]): any.ApplyExpression[config.baseFinalTypes.type] =
      applyExpression(typeReferenceExpression(typeApplication(leafTpe(), Seq(tpe))), Seq(value))

    implicit def convert(other: NodeTpe[this.type]): NodeTpe[this.type]
    implicit def convert(other: LeafTpe[this.type]): LeafTpe[this.type]
    implicit def convert(other: CreateNodeExpr[this.type]): CreateNodeExpr[this.type]
  }
  
  trait Config {
    type BFT <: OperatorExpressionOps.FinalTypes & polymorphism.FinalTypes
    val baseFinalTypes: BFT
    val baseFactory: OperatorExpressionOps.Factory[baseFinalTypes.type] & polymorphism.Factory[baseFinalTypes.type]

    type FT <: FinalTypes[baseFinalTypes.type]
    val finalTreeTypes: FT
    val factory: Factory[finalTreeTypes.type]
  }
}
