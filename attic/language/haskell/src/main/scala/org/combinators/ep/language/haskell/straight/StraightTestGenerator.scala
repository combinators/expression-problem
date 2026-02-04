package org.combinators.ep.language.haskell.straight   /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.haskell.{HUnitTestGenerator, Haskell, HaskellWithPath}
import org.combinators.ep.domain.BaseDomain

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait StraightTestGenerator extends HUnitTestGenerator {
  val domain: BaseDomain with ModelDomain

  val flat:domain.Model

  /** Combine all test cases together into a single Haskell file. */
  override def generateSuite(pkg: Option[String]): Seq[HaskellWithPath] = {
    val opsImports = flat.ops.map(op => s"import ${op.concept}").mkString("\n")

    testGenerator.zipWithIndex.map{ case (md, num) => {
      HaskellWithPath(Haskell(s"""|module Main where
                                  |import Test.HUnit
                                  |import DataTypes
                                  |
                                  |$opsImports
                                  |${md.mkString("\n")}
                                  |""".stripMargin), Paths.get(s"Main$num.hs"))
    }}

  }
}