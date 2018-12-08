package example.expression.haskell.grow    /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.haskell.{HUnitTestGenerator, Haskell, HaskellWithPath}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait GrowTestGenerator extends HUnitTestGenerator with GrowGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  val flat:domain.Model

  /** Find the model which contains a given atomic inst. */
  def findModel (exp:AtomicInst) : Model = {
    getModel.toSeq.filter(m => m.types.contains(exp.e)).head
  }

  /**
    * Convert the given atomic instance, and use base as the variable name for all interior expansions.
    *
    * Need to find EVOLUTION in which an operation was defined (other than M0) so you can call the
    * appropriate M*Ext to lift up for types
    */
  override def convert(inst:AtomicInst) : Haskell = {
    val name = inst.e.name
    val model = findModel(inst)

    // For the base (and one above it), there is no need to wrap, otherwise must wrap
    val wrap = if (model.base() == model) {
      (s:String) => s
    } else {
      (s:String) => {
        model.last.inChronologicalOrder.reverse.tail.foldLeft(s"${extDeclaration(model.last)} ($s)")((former,tail) =>
          s"(${extDeclaration(tail)} ($former))")
      }
    }

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

      val id: String = s"v$idx"

      // test_e3_1 = TestCase (assertEqual "NegCheck-Eval" (0-5.0) (${Eval.name} n1))
      // (evalExpM0 (Add (Lit 1.0) (Lit 2.0))  ))
      val disp = s"(${test.op.name}${domain.baseTypeRep.name}${model.name.capitalize} (${convert(test.inst)}))"
      //val disp = dispatch(convert(test.inst), test.op)

      expected(test, id)(expectedExpr => Seq(new Haskell(s"""test_$id = TestCase (assertEqual "${test.getClass.getSimpleName}" $expectedExpr $disp)""")))
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