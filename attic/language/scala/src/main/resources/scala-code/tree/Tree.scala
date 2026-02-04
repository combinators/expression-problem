package org.combinators.ep.util      /*DI:LI:AI*/

sealed trait Tree
case class Node(label: Int, children: Seq[Tree]) extends Tree
case class Leaf[T](value: T) extends Tree
