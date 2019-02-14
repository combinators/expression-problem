package org.combinators.ep.language.haskell.alacarte  /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.haskell.{HUnitTestGenerator, Haskell, HaskellType, HaskellWithPath}
import org.combinators.ep.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait ALaCarteTestGenerator extends HUnitTestGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  val flat:domain.Model

  /** normalize the atomic Instance by its position within flattened data types. */
  def treeRoute(a:AtomicInst, flattened:Seq[Atomic]) : String = {
    if (flattened.size == 1) {
      s"${a.e.concept}"
    } else if (a.e == flattened.head) {
      s"El(${a.e.concept} "
    } else {
      "Er(" + treeRoute(a, flattened.tail) + " "
    }
  }

  // ugly! works, though...
  def closeTreeRoute(a:AtomicInst, flattened:Seq[Atomic]) : String = {
    if (flattened.size == 1) {
      ""
    } else if (a.e == flattened.head) {
      ")"
    } else {
      ")" + closeTreeRoute(a, flattened.tail)
    }
  }

  /**
    * Expand instance into its post-order traversal of interior definitions.
    *
    * a1 = (7*2)
    *
    * a1 = In(Er(Er(BinaryMul In(El(Constant 7.0))  In(El(Constant 2.0)))))
    */
  override def convert(inst:Inst) : Haskell = {
    Haskell(convert0(inst).getCode + ":: GeneralExpr")
  }

  /** Recursive helper method. Creates the prefix In(Er(El(... Followed by dataType name. */
  def convert0(inst:Inst) : Haskell = {
    val name = inst.name
    inst match {
      case ui: UnaryInst =>
        Haskell(s"In(" + treeRoute(ui, flat.types) + s" (${convert0(ui.inner)}) " + closeTreeRoute(inst, flat.types) + ")")

      case bi: BinaryInst =>
          Haskell(s"In(" + treeRoute(bi, flat.types) + s" (${convert0(bi.left)}) (${convert0(bi.right)}) " + closeTreeRoute(inst, flat.types) + ")")

      case exp: AtomicInst =>
        Haskell(s"In(" + treeRoute(exp, flat.types) + exp.ei.inst + closeTreeRoute(inst, flat.types) + ")")

      case _ => Haskell(s""" -- unknown $name" """)
    }
  }

  // TODO: issues with other haskell implementations. must move to subclasses
  override def generateDataTypes(m:domain.Model): HaskellWithPath = {
    val allTypes = m.types.map(exp => {
      val params:Seq[HaskellType] = exp.attributes.map(att => typeConverter(att.tpe))
      val list:String = params.map(f => f.toString).mkString(" ")
      Haskell(s"${exp.concept}T $list") // not sure how much this is needed
    }).mkString("  | ")

    val binaryTreeInterface =  if (m.flatten().hasBinaryMethod()) {
      // astree method declaration
      definedDataSubTypes("", m.types) ++ declarations
    } else {
      Seq.empty
    }

    val code = Haskell(
      s"""|module DataTypes where
          |import GeneralExpr
          |${binaryTreeInterface.mkString("\n")}
          |
          |-- All types are classified as data
          |data ${domain.baseTypeRep.name} = $allTypes
          |""".stripMargin)

    HaskellWithPath(code, Paths.get("DataTypes.hs"))
  }

  /** Create multiple Haskell files for test cases. */
  override def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath] = {
    val opsImports = flat.ops.map(op => s"import ${op.concept}").mkString("\n")
    val typesImports = flat.types.map(exp => s"import ${exp.concept}").mkString("\n")
    var num: Int = -1

    testGenerator.map(md => {
      num = num + 1
      HaskellWithPath(Haskell(s"""|module Main where
                                  |import Test.HUnit
                                  |import GeneralExpr
                                  |import Base
                                  |
                                  |$opsImports
                                  |$typesImports
                                  |$md""".stripMargin), Paths.get(s"Main$num.hs"))
    })
  }
}