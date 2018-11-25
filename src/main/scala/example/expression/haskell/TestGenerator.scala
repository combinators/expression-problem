package example.expression.haskell    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

trait TestGenerator {
  val domain: BaseDomain with ModelDomain

  /** Return sample test cases as method. */
  def testGenerator: Seq[Haskell] = Seq.empty
}
