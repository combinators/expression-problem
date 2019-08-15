package org.combinators.ep.domain    /*DI:LI:AI*/

/** Scala Dependent Pair Type is a dependent sum, Sigma, where there exists a type and an instance for that type.
  * Check https://partialflow.wordpress.com/2017/07/26/dependent-types-type-level-programming/ for an introduction
  * on dependent types in Scala.
  *
  * The type of the instance is captured by 'tpe'
  */
sealed trait ExistsInstance {
  val tpe : TypeRep
  val inst: tpe.scalaInstanceType
}

/** Companion object to create Dependent Pairs. */
object ExistsInstance {
  type Aux[T] = ExistsInstance { val tpe: TypeRep { type scalaInstanceType = T } }
  def apply(tpe: TypeRep)(inst: tpe.scalaInstanceType): ExistsInstance = {
    val tpeArg: tpe.type = tpe
    val instArg: tpeArg.scalaInstanceType = inst
    new ExistsInstance {
      val tpe: tpeArg.type = tpeArg
      val inst: tpeArg.scalaInstanceType = instArg
    }

  }
}