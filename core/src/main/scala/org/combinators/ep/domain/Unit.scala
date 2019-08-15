package org.combinators.ep.domain    /*DI:LI:AI*/

// Operations that have no return, will be marked as Void
case object Unit extends TypeRep {
  override type scalaInstanceType = scala.Unit
}
