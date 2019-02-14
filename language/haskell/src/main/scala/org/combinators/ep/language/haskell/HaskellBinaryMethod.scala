package org.combinators.ep.language.haskell    /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}

trait HaskellBinaryMethod {
  val domain: BaseDomain with ModelDomain

  type Declaration = Haskell

  /**
    * Declares the helper concepts needed.
    * @return
    */
  def declarations: Seq[Declaration] = {
    Seq(Haskell(
      s"""
         |-- tree definition
         |data Tree = Node DeclaredTypes [ Tree ] | Leaf Double deriving (Eq, Show)
         |""".stripMargin))
  }

  /**
    * Add defined data types for given exp subtype
    * @param context
    * @param exps
    */
  def definedDataSubTypes(context:String, exps:Seq[domain.DataType]) :Seq[Declaration] = {
    val types = exps.map(exp => exp.concept + "Type").mkString("|")
    Seq(Haskell(s"data DeclaredTypes = $types deriving (Eq, Show)"))
  }

}
