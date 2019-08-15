package org.combinators.ep.domain

import org.combinators.ep.domain.abstractions.TypeRep

/*DI:LI:AI*/

/** Scala Dependent Pair Type is a dependent sum, Sigma, where there exists a type and an instance for that type.
  * Check https://partialflow.wordpress.com/2017/07/26/dependent-types-type-level-programming/ for an introduction
  * on dependent types in Scala.
  *
  * The type of the instance is captured by 'tpe'
  */
sealed trait ExistsInstance {
  val tpe : TypeRep
  val inst: tpe.HostType
}

/** Companion object to create Dependent Pairs. */
object ExistsInstance {
  type Aux[T] = ExistsInstance { val tpe: TypeRep.OfHostType[T] }
  def apply(tpe: TypeRep)(inst: tpe.HostType): ExistsInstance = {
    val tpeArg: tpe.type = tpe
    val instArg: tpeArg.HostType = inst
    new ExistsInstance {
      val tpe: tpeArg.type = tpeArg
      val inst: tpeArg.HostType = instArg
    }
  }
}