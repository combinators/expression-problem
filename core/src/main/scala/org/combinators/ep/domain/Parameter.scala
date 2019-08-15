package org.combinators.ep.domain    /*DI:LI:AI*/

/**
  * An operation can have a number of parameters, each of which has a name and a type
  * @param n       name of parameter
  * @param tpe     its type
  */
case class Parameter(n:String, tpe:TypeRep) extends Element {
  val name: String = n
}
