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
}
