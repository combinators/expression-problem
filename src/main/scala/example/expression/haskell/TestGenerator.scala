package example.expression.haskell    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}



/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  /** Return sample HUnit test cases. */
  def testGenerator: Seq[Haskell] = Seq.empty

  /** Create multiple Haskell files for test cases. */
  def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath]

  /** Convert the given atomic instance, and use base as the variable name for all interior expansions. */
  def convert(base:String, inst:AtomicInst) : Seq[Haskell]

}