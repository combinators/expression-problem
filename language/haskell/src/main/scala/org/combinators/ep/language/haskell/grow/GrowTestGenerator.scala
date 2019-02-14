package org.combinators.ep.language.haskell.grow    /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.haskell.{HUnitTestGenerator, Haskell, HaskellWithPath}
import org.combinators.ep.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait GrowTestGenerator extends HUnitTestGenerator with GrowGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  val flat:domain.Model

  /**
    * Convert the given atomic instance, and use base as the variable name for all interior expansions.
    *
    * Need to find EVOLUTION in which an operation was defined (other than M0) so you can call the
    * appropriate M*Ext to lift up for types
    */
  override def convert(inst:Inst) : Haskell = {
    val name = inst.name

    // For the base (and one above it), there is no need to wrap, otherwise must wrap

    inst match {
      case ui: UnaryInst =>
        val wrap = genWrap(findModel(ui.e))
        Haskell(wrap(s"${ui.e.concept} (${toTargetLanguage(ui.inner)}) "))

      case bi: BinaryInst =>
        val wrap = genWrap(findModel(bi.e))
        Haskell(wrap(s"${bi.e.concept} (${toTargetLanguage(bi.left)}) (${toTargetLanguage(bi.right)}) "))

      case exp: AtomicInst =>
        val wrap = genWrap(findModel(exp.e))
        Haskell(wrap(s"${exp.e.concept} ${exp.ei.inst}"))

      case _ => Haskell(s""" -- unknown $name" """)
    }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath] = {
    val opsImports = model.get.toSeq.filterNot(m => m.isEmpty).map(m => s"import ${m.name.capitalize}").reverse.mkString("\n")
    var num: Int = -1
    val files: Seq[HaskellWithPath] = testGenerator.map(md => {
      num = num + 1
      HaskellWithPath(Haskell(s"""|module Main where
                                  |import Test.HUnit
                                  |
                                  |$opsImports
                                  |$md""".stripMargin), Paths.get(s"Main$num.hs"))
    })

    files
  }
}
