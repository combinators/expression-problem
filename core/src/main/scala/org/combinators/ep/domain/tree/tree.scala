package org.combinators.ep.domain.tree     /*DI:LI:AI*/

/** Provides models of generic trees. */
import org.combinators.ep.domain.instances.InstanceRep

/** Models a generic int-labeled tree with data of any type stored in its leaves. */
sealed trait Tree

/** Models inner tree nodes with integer labels. */
case class Node(label: Int, children: Seq[Tree]) extends Tree

/** Models leaf nodes with any type of data attached to them. */
case class Leaf(value: InstanceRep) extends Tree
