package org.combinators.ep.domain    /*DI:LI:AI*/

/** There is a base type and subsequent sub-types will extend Types. */
// TODO: Technically this could/should be part of the language specific generations
trait TypeRep {
  type scalaInstanceType

  def name: String = getClass.getName

  /**
    * Request the type as an instance, such as "exp" for the Exp domain.
    *
    * This is useful, for example, for the etiquette of lower case for method names.
    */
  def instance : String = name.toLowerCase

  /**
    * Request the operation as a concept, such as "Exp" for the Exp domain.
    *
    * This is useful, for example, for the etiquette of capitalizing class and interface names.
    */
  def concept : String = name.capitalize
}

/**
  * A companion object for the TypeRep which declares a type for TypeRep which have a generic
  * parameter for its scala instance type.
  */
object TypeRep {
  type Aux[T] = TypeRep { type scalaInstanceType = T }
}

