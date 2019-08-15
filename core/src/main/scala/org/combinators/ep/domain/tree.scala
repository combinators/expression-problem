package org.combinators.ep.domain

package object tree {
  sealed trait Tree
  case class Node(label: Int, children: Seq[Tree]) extends Tree
  case class Leaf(value: Any) extends Tree
}