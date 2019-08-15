package org.combinators.ep.domain    /*DI:LI:AI*/

case class Attribute(n:String, tpe:TypeRep) extends Element {
  val name:String = n

  /**
    * Request the operation as an instance, such as "eval" for the Eval operation.
    *
    * This is useful, for example, for the etiquette of lower case for method names.
    */
  def instance : String = name.toLowerCase

  /**
    * Request the operation as a concept, such as "Eval" for the Eval operation.
    *
    * This is useful, for example, for the etiquette of capitalizing class and interface names.
    */
  def concept : String = name.capitalize
}
