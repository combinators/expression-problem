package ep.haskell    /*DI:LD:AI*/

import ep.domain.{BaseDomain, ModelDomain}

trait TestGenerator {
  val domain: BaseDomain with ModelDomain

  /** Return sample test cases as method. */
  def testGenerator: Seq[Haskell] = Seq.empty
}
