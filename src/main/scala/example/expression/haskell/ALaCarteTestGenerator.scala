package example.expression.haskell    /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait ALaCarteTestGenerator extends TestGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  val flat:domain.Model

  /** normalize the atomic Instance by its position within flattened data types. */
  def treeRoute(a:AtomicInst, flattened:Seq[Atomic]) : String = {
    if (flattened.size == 1) {
      s"${a.e.name.capitalize}"
    } else if (a.e == flattened.head) {
      s"El(${a.e.name.capitalize} "
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
  override def convert(inst:AtomicInst) : Haskell = {
    Haskell(convert0(inst).getCode + ":: GeneralExpr")
  }

  /** Recursive helper method. Creates the prefix In(Er(El(... Followed by dataType name. */
  def convert0(inst:AtomicInst) : Haskell = {
    val name = inst.e.name
    inst match {
      case ui: UnaryInst =>
        Haskell(s"In(" + treeRoute(inst, flat.types) + s" (${convert0(ui.inner)} " + closeTreeRoute(inst, flat.types) + ")")

      case bi: BinaryInst =>
          Haskell(s"In(" + treeRoute(inst, flat.types) + s" (${convert0(bi.left)}) (${convert0(bi.right)}) " + closeTreeRoute(inst, flat.types) + ")")

      case exp: AtomicInst =>
        Haskell(s"In(" + treeRoute(inst, flat.types) + exp.i.get + closeTreeRoute(inst, flat.types) + ")")

      case _ => Haskell(s""" -- unknown $name" """)
    }
  }

  /** Create multiple Haskell files for test cases. */
  override def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath] = {
    val opsImports = flat.ops.map(op => s"import ${op.name.capitalize}").mkString("\n")
    val typesImports = flat.types.map(exp => s"import ${exp.name.capitalize}").mkString("\n")
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