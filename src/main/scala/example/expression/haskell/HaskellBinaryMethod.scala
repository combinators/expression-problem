package example.expression.haskell

import example.expression.generator.BinaryMethod

trait HaskellBinaryMethod extends BinaryMethod {
  type Declaration = Haskell

  /**
    * Declares the helper concepts needed.
    * @return
    */
  def declarations: Seq[Declaration] = {
    Seq.empty
  }

  /**
    * Add defined data types for given exp subtype
    * @param context
    * @param exps
    */
  def definedDataSubTypes(context:String, exps:Seq[domain.Atomic]) :Seq[Declaration] = {
    val types = exps.map(exp => exp.name.capitalize + "Type").mkString("|")
    val treeDef = Haskell("data Tree = Nil | Leaf Subtype Double | Node Subtype Exp Tree Tree")
    Seq(Haskell(s"data Subtype = $types"), treeDef)
  }
}
