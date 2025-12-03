package org.combinators.ep.builder.scala.paradigm.ffi

import org.combinators.cogen.{FileWithPath, TypeRep}
import org.combinators.ep.builder.inbetween.paradigm.ffi.TreesAST as InbetweenTreesAST
import org.combinators.ep.domain.abstractions.DomainTpeRep
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}

import java.nio.file.Paths

trait TreesAST extends InbetweenTreesAST { self: ParametricPolymorphismAST & BaseAST =>
  object scalaTreesOps {
    object treesOpsOverride {
      trait FinalTypes extends treesOps.FinalTypes {
        type Tree <: treesOpsOverride.Tree
        type Leaf <: treesOpsOverride.Leaf
        type Node <: treesOpsOverride.Node
      }
      
      trait Tree extends treesOps.Tree with scalaBase.ooOverrides.ClassReferenceType {
        def qualifiedClassName: Seq[any.Name] = Seq("org", "combinators", "ep", "util", "Tree").map(n => scalaBaseFactory.name(n, n))
      }

      trait Leaf extends treesOps.Leaf with scalaBase.ooOverrides.ClassReferenceType {
        def qualifiedClassName: Seq[any.Name] = Seq("org", "combinators", "ep", "util", "Leaf").map(n => scalaBaseFactory.name(n, n))
      }

      trait Node extends treesOps.Node with scalaBase.ooOverrides.ClassReferenceType {
        def qualifiedClassName: Seq[any.Name] = Seq("org", "combinators", "ep", "util", "Node").map(n => scalaBaseFactory.name(n, n))
      }

      trait Factory extends treesOps.Factory {}
    }

    def treeReificationExtensions(tpe: TypeRep)(value: tpe.HostType): Option[String] = {
      tpe match {
        case t: DomainTpeRep.Tree.type =>
          value match {
            case org.combinators.ep.domain.tree.Node(id, values) => Some(s"org.combinators.ep.util.Node($id, ${values.map(v => scalaBaseFactory.reifiedScalaValue(DomainTpeRep.Tree, v).toScala).mkString(", ")})")
            case org.combinators.ep.domain.tree.Leaf(r) => Some(s"org.combinators.ep.util.Leaf(${scalaBaseFactory.reifiedScalaValue(r.tpe, r.inst).toScala})")
          }
        case _ => None
      }
    }

    def treeLibrary: FileWithPath = {
      FileWithPath(
        getClass.getResourceAsStream("/scala-code/org/combinators/ep/util/Trees.scala").readAllBytes(),
        Paths.get("src", "main", "scala", "org", "combinators", "ep", "util", "Trees.scala")
      )
    }
  }

  override val treesOpsFinalTypes: scalaTreesOps.treesOpsOverride.FinalTypes
  override val treesOpsFactory: scalaTreesOps.treesOpsOverride.Factory
}

trait FinalTreesAST extends TreesAST { self: FinalBaseAST =>
  object treesFinalTypes {
    trait FinalTrees extends scalaTreesOps.treesOpsOverride.FinalTypes {
      type Tree = scalaTreesOps.treesOpsOverride.Tree
      type Leaf = scalaTreesOps.treesOpsOverride.Leaf
      type Node = scalaTreesOps.treesOpsOverride.Node
    }
  }

  object finalTreesFactoryTypes {
    trait FinalTreesFactory extends scalaTreesOps.treesOpsOverride.Factory {
      def tree(): treesOps.Tree = {
        case class Tree() extends scalaTreesOps.treesOpsOverride.Tree {
          override def getSelfTree: treesOpsFinalTypes.Tree = this
          def getSelfClassReferenceType: scalaBase.ooOverrides.ClassReferenceType = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        Tree()
      }
      def node(): treesOps.Node = {
        case class Node() extends scalaTreesOps.treesOpsOverride.Node {
          override def getSelfNode: treesOpsFinalTypes.Node = this
          def getSelfClassReferenceType: scalaBase.ooOverrides.ClassReferenceType = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        Node()
      }
      def leaf(): treesOps.Leaf = {
        case class Leaf() extends scalaTreesOps.treesOpsOverride.Leaf {
          override def getSelfLeaf: treesOpsFinalTypes.Leaf = this
          def getSelfClassReferenceType: scalaBase.ooOverrides.ClassReferenceType = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        Leaf()
      }
    }
  }

  override val treesOpsFinalTypes: treesFinalTypes.FinalTrees = new treesFinalTypes.FinalTrees {}
  override val treesOpsFactory: finalTreesFactoryTypes.FinalTreesFactory = new finalTreesFactoryTypes.FinalTreesFactory {}
}