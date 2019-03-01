package org.combinators.ep.language.haskell.straight   /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.haskell.{HUnitTestGenerator, Haskell, HaskellWithPath}
import org.combinators.ep.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait StraightTestGenerator extends HUnitTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  val flat:domain.Model

//  /** Convert the given atomic instance, and use base as the variable name for all interior expansions. */
//  override def convert(inst:Inst) : Haskell = {
//    val name = inst.name
//    inst match {
//      case ui: UnaryInst =>
//        Haskell(s"${ui.e.concept} (${toTargetLanguage(ui.inner)}) ")
//
//      case bi: BinaryInst =>
//        Haskell(s"${bi.e.concept} (${toTargetLanguage(bi.left)}) (${toTargetLanguage(bi.right)}) ")
//
//      case exp: AtomicInst =>
//        Haskell(s"${exp.e.concept} ${exp.ei.inst}")
//
//      case _ => Haskell(s""" -- unknown $name" """)
//    }
//  }

  /** Combine all test cases together into a single Haskell file. */
  override def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath] = {
    val opsImports = flat.ops.map(op => s"import ${op.concept}").mkString("\n")

    testGenerator.zipWithIndex.map{ case (md, num) => {
      HaskellWithPath(Haskell(s"""|module Main where
                                  |import Test.HUnit
                                  |import DataTypes
                                  |
                                  |$opsImports
                                  |$md""".stripMargin), Paths.get(s"Main$num.hs"))
    }}

  }
}