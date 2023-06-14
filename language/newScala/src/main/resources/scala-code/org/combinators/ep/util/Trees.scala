package org.combinators.ep.util

trait Tree {}
case class Leaf[T](element: T) extends Tree
case class Node(label: String, children: Tree*) extends Tree