package org.combinators.ep.builder.inbetween.paradigm.ffi

import org.combinators.ep.language.inbetween.ffi.OperatorExpressionOpsAST
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait TreesAST extends ParametricPolymorphismAST {
  object treesOps {
    trait FinalTypes {
      type Tree <: treesOps.Tree
      type Leaf <: treesOps.Leaf
      type Node <: treesOps.Node
    }
    
    trait Tree extends any.Type {
      def getSelfTree: treesOpsFinalTypes.Tree
    }

    trait Leaf extends any.Type {
      def getSelfLeaf: treesOpsFinalTypes.Leaf
    }

    trait Node extends any.Type {
      def getSelfNode: treesOpsFinalTypes.Node
    }

    trait Factory {
      def tree(): Tree
      def node(): Node
      def leaf(): Leaf

      def createNode(label: any.Expression, children: Seq[any.Expression]): any.ApplyExpression =
        factory.applyExpression(polymorphismFactory.typeReferenceExpression(node()), label +: children)

      def createLeaf(tpe: any.Type, value: any.Expression): any.ApplyExpression =
        factory.applyExpression(polymorphismFactory.typeReferenceExpression(polymorphismFactory.typeApplication(leaf(), Seq(tpe))), Seq(value))

      implicit def convert(other: Tree): treesOpsFinalTypes.Tree = other.getSelfTree
      implicit def convert(other: Leaf): treesOpsFinalTypes.Leaf = other.getSelfLeaf
      implicit def convert(other: Node): treesOps.Node = other.getSelfNode
    }
  }
  val treesOpsFinalTypes: treesOps.FinalTypes
  val treesOpsFactory: treesOps.Factory
}