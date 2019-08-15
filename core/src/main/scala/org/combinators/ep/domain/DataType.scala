package org.combinators.ep.domain    /*DI:LI:AI*/

/** Pre-defined unary/binary subtypes that reflects either a unary or binary structure. This is extensible. */
abstract class DataType(raw:String, val attributes: Seq[Attribute]) {
  val name:String = raw

  /**
    * Request the data-type as an instance, such as "add" for the Add data type.
    *
    * This is useful, for example, for the etiquette of lower case for methods and attribute names.
    */
  def instance : String = name.toLowerCase

  /**
    * Request the data-type name as a concept, such as "Add" for Add data type.
    *
    * This is useful, for example, for the etiquette of capitalizing class names.
    */
  def concept : String = name.capitalize
}
