package example.expression.haskell.grow  /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.haskell.{Haskell, HaskellProducer}

/**
  * Any Haskell-based approach capable of supporting Producer must provide this capability.
  */
trait GrowProducer extends GrowGenerator  {
//  val domain: BaseDomain with ModelDomain
//  import domain._
//
//  /**
//    * For producer operations, there is a need to instantiate objects, and one would use this
//    * method (with specific parameters) to carry this out.
//    */
//  override def inst(exp:domain.Atomic, params:Haskell*): Haskell = {
//
//   val wrap = genWrap(findModel(exp))
//   exp match {
//      case ui: Unary =>
//        Haskell(wrap(s"${ui.name.capitalize} (${params(0)}) "))
//
//      case bi: Binary =>
//        Haskell(wrap(s"${bi.name.capitalize} (${params(0)}) (${params(1)}) "))
//
//      case exp: Atomic =>
//        Haskell(wrap(s"${exp.name.capitalize} ${params(0)} "))
//
//      case _ => Haskell(s" -- unknown ${exp.name} ")
//    }
//  }
//
//  override def typeSignature(m:Model, op:Operation) : String = {
//    op match {
//      case _:ProducerOperation =>
//        val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
//        val baseDomain = domain.baseTypeRep.name
//
//        s"${op.name}$baseDomain$mcaps :: ${expDeclaration(m.base())} $mcaps -> ${expDeclaration(m.base())} $mcaps"
//
//      case _ => super.typeSignature(m, op)
//    }
//  }
}
