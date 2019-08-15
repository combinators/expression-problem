package org.combinators.ep.domain    /*DI:LI:AI*/

/** Each operation is named and has parameters and return type, which defaults to Void (no return value). */
abstract class Operation(n1:String, val returnType:TypeRep = Unit, val parameters:Seq[Parameter] = Seq.empty) extends Element {
  val name:String = n1

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