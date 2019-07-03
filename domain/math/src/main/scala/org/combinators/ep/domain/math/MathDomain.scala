package org.combinators.ep.domain.math        /*DD:LI:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}

/**
  * Mathematical Expressions domain as an instance of EP.
  */
trait MathDomain extends BaseDomain with ModelDomain {

  case object Exp extends TypeRep {
    type scalaInstanceType = Inst

    override def name:String = "Exp"
  }
  type BaseTypeRep = Exp.type
  val baseTypeRep:BaseTypeRep = Exp
}

/** Companion object to represent domain. */
object MathDomain extends MathDomain
