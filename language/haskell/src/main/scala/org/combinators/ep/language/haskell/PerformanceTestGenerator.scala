package org.combinators.ep.language.haskell   /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator

/**
  * Skipping for now, until we have some haskell examples of performance
  */
trait PerformanceTestGenerator extends HaskellGenerator with LanguageIndependentTestGenerator with TestGenerator  {
  val domain: BaseDomain with ModelDomain
  import domain._

  override def hunitTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
    test match {
      case _: PerformanceTestCase =>
        CodeBlockWithResultingExpressions(HaskellStatement(s"""test_v$idx = TestCase (assertEqual "Skipping-Performance" 1 1)"""))().block

      case _ => super.hunitTestMethod(test, idx)
    }
  }
}
