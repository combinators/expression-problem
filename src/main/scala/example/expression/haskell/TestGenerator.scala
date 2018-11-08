package example.expression.haskell

import example.expression.domain.{BaseDomain, ModelDomain}

trait TestGenerator {
  val domain: BaseDomain with ModelDomain

  /** Return sample test cases as method. */
  def testGenerator: Seq[Haskell] = Seq.empty
}
