package org.combinators.ep.language.haskell.grow    /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.haskell.{HUnitTestGenerator, Haskell, HaskellWithPath}
import org.combinators.ep.domain.BaseDomain

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait GrowTestGenerator extends HUnitTestGenerator with GrowGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  val flat:domain.Model


  /**
    * Actual value in a test case.
    */
  // TODO: OVERRIDE HERE

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String]): Seq[HaskellWithPath] = {
    val opsImports = getModel.toSeq.filterNot(m => m.isEmpty).map(m => s"import ${m.name.capitalize}").reverse.mkString("\n")
    var num: Int = -1
    val files: Seq[HaskellWithPath] = testGenerator.map(md => {
      num = num + 1
      HaskellWithPath(Haskell(s"""|module Main where
                                  |import Test.HUnit
                                  |
                                  |$opsImports
                                  |${md.mkString("\n")}
                                  |""".stripMargin), Paths.get(s"Main$num.hs"))
    })

    files
  }
}
