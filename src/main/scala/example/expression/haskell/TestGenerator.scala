package example.expression.haskell

/*DI:LD:AI*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  val flat:domain.Model

  /** Return sample JUnit test cases. */
  def testGenerator: Seq[Haskell] = Seq.empty

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
//
//  /** normalize the atomic Instance by its position within flattened data types. */
//  def treeRouteRight(a:AtomicInst, flattened:Seq[Atomic]) : String = {
//    if (a.e == flattened.head) {
//      s"${a.e.name.capitalize} "
//    } else {
//      "Er(" + treeRouteRight(a, flattened.tail) + " "
//    }
//  }

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

  def closeTreeRouteRight(a:AtomicInst, flattened:Seq[Atomic]) : String = {
    if (a.e == flattened.head) {
      ""
    } else {
      ")" + closeTreeRouteRight(a, flattened.tail)
    }
  }

  /**
    * Expand instance into its post-order traversal of interior definitions.
    *
    * a1 = (7*2)+8
    *
    * a1LL = In(El(Constant 7.0)) :: GeneralExpr
    * a1LR = In(El(Constant 2.0)) :: GeneralExpr
    * a1R = In(El(Constant 8.0)) :: GeneralExpr
    * a1L = In(Er(Er(BinaryMul a1LL a1LR))) :: GeneralExpr
    * a1 = In(Er(El(BinaryPlus a1L a1R))) :: GeneralExpr

    */
  def expand(base:String, inst:AtomicInst) : Seq[Haskell] = {
    val name = inst.e.name
    inst match {
      case ui: UnaryInst =>
        expand(base + "L", ui.inner) :+
          Haskell(s"$base = In(" + treeRoute(inst, flat.types) + s"${base}L" + closeTreeRoute(inst, flat.types) + ")")

      case bi: BinaryInst =>
        expand(base + "L", bi.left) ++ expand(base + "R", bi.right) :+
          Haskell(s"$base = In(" + treeRoute(inst, flat.types) + s"${base}L ${base}R " + closeTreeRoute(inst, flat.types) + ")")

      case exp: AtomicInst =>
        Seq(Haskell(s"$base = In(" + treeRoute(inst, flat.types) + exp.i.get + closeTreeRoute(inst, flat.types) + ")"))

      case _ => Seq(Haskell(s""" -- unknown $name" """))
    }
  }


  /**
    * Convert a test instance into a Haskell Type Tree expression. Pre-pend inner definitions and append aggregate
    * since there doesn't seem to be any way to make this a one-liner
    */
  def convert(inst: AtomicInst): Haskell = {
    val name = inst.e.name
    val location = treeRoute(inst, flat.types)
    inst match {
      case ui: UnaryInst =>
        Haskell(s"new $name(${convert(ui.inner)})")

      case bi: BinaryInst =>
        val left = convert(bi.left)
        val right = convert(bi.right)
        Haskell(s"$location ($left, $right)")

      case exp: AtomicInst => Haskell(s"$location ${exp.i.get})")   // extra closing ")" for recursion

      case _ => Haskell(s""" "unknown $name" """)
    }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath] = {
    val opsImports = flat.ops.map(op => s"import ${op.name.capitalize}").mkString("\n")
    val typesImports = flat.types.map(exp => s"import ${exp.name.capitalize}").mkString("\n")
    var num: Int = 0
    val files: Seq[HaskellWithPath] = testGenerator.map(md => {
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

    files
  }
}