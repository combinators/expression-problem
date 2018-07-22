package example.expression.haskell    /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait StraightTestGenerator extends TestGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  val flat:domain.Model

  /** Convert the given atomic instance, and use base as the variable name for all interior expansions. */
  override def convert(base:String, inst:AtomicInst) : Seq[Haskell] = {
    val name = inst.e.name
    inst match {
      case ui: UnaryInst =>
        convert(base + "L", ui.inner) :+
          Haskell(s"$base = ${ui.e.name.capitalize} ${base}L  ")

      case bi: BinaryInst =>
        convert(base + "L", bi.left) ++ convert(base + "R", bi.right) :+
          Haskell(s"$base = ${bi.e.name.capitalize} ${base}L ${base}R ")

      case exp: AtomicInst =>
        Seq(Haskell(s"$base = ${exp.e.name.capitalize} ${exp.i.get}"))

      case _ => Seq(Haskell(s""" -- unknown $name" """))
    }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath] = {
    val opsImports = flat.ops.map(op => s"import ${op.name.capitalize}").mkString("\n")
    var num: Int = -1
    val files: Seq[HaskellWithPath] = testGenerator.map(md => {
      num = num + 1
      HaskellWithPath(Haskell(s"""|module Main where
                                  |import Test.HUnit
                                  |import DataTypes
                                  |
                                  |$opsImports
                                  |$md""".stripMargin), Paths.get(s"Main$num.hs"))

    })

    files
  }
}