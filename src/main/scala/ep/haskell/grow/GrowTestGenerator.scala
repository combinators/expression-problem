package ep.haskell.grow    /*DI:LD:AD*/

import java.nio.file.Paths

import ep.domain.{BaseDomain, ModelDomain}
import ep.haskell.{HUnitTestGenerator, Haskell, HaskellWithPath}

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
  override def convert(inst:AtomicInst) : Haskell = {
    val name = inst.e.name

    // For the base (and one above it), there is no need to wrap, otherwise must wrap
    val wrap = genWrap(findModel(inst.e))

    inst match {
      case ui: UnaryInst =>
        Haskell(wrap(s"${ui.e.name.capitalize} (${convert(ui.inner)}) "))

      case bi: BinaryInst =>
        Haskell(wrap(s"${bi.e.name.capitalize} (${convert(bi.left)}) (${convert(bi.right)}) "))

      case exp: AtomicInst =>
        Haskell(wrap(s"${exp.e.name.capitalize} ${exp.i.get}"))

      case _ => Haskell(s""" -- unknown $name" """)
    }
  }

  /** RMore complicated invocation. */
  override def hunitMethod(tests:Seq[TestCase]) : Haskell = {
    val model = getModel
    val stmts: Seq[Haskell] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1
      val idx = pair._2

      test match {
        case eq: EqualsTestCase =>
          // test_e3_1 = TestCase (assertEqual "NegCheck-Eval" (0-5.0) (${Eval.name} n1))
          // (evalExpM0 (Add (Lit 1.0) (Lit 2.0))  ))
          val disp = s"(${eq.op.name}${domain.baseTypeRep.name}${model.name.capitalize} (${convert(eq.inst)}))"
          //val disp = dispatch(convert(test.inst), test.op)

          expected(eq, idx)(expectedExpr => Seq(new Haskell(s"""test_v$idx = TestCase (assertEqual "${test.getClass.getSimpleName}" $expectedExpr $disp)""")))

        case seq:EqualsCompositeTestCase =>
          val x :Expression = actual(seq.ops.head, seq.inst)   // HACK: Only works for two-deep
          val y :Expression = dispatch(x, seq.ops.tail.head)
          expected(seq, idx)(expectedExpr => Seq(new Haskell(s"""test_v$idx = TestCase (assertEqual "${test.getClass.getSimpleName}" ($expectedExpr) $y)""")))
      }
    })

    val structure = tests.zipWithIndex.map(pair => {
      val idx = pair._2
      new Haskell(s"""TestLabel "$idx" test_v$idx""")
    }).mkString(",")

    new Haskell(s"""|${stmts.mkString("\n")}
                    |test_all = TestList [ $structure ]
                    |
                    |main :: IO Counts
                    |main  = runTestTT test_all""".stripMargin)
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