package org.combinators.ep.domain.shape    /*DD:LI:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}


/**
  * Shape domain as suitable for
  *
  * Synthesizing Object-Oriented and Functional Design to Promote Re-Use
  * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
  *
  * https://www.cs.rice.edu/~cork/teachjava/2003/readings/visitor1.pdf
  */
trait ShapeDomain extends BaseDomain with ModelDomain {

  case object Shape extends TypeRep {
    type scalaInstanceType = Inst
    override def name: String = "Shape"
  }
  type BaseTypeRep = Shape.type
  val baseTypeRep:BaseTypeRep = Shape
}

/** Companion object to represent domain. */
object ShapeDomain extends ShapeDomain
