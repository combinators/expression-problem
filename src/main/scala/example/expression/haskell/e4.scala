package example.expression.haskell

/*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends Evolution with AbstractGenerator with TestGenerator with Producer with M0 with M1 with M2 with M3 with M4 {
  self:e0 with e1 with e2 with e3 =>
  val domain:MathDomain

  /** If any new imports are needed for an operation, just extend here. */
  override def addedImports(op:domain.Operation):Seq[Haskell] = {
    op match {
      case Simplify => Seq(Haskell("import Eval"))
      case _ => super.addedImports(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case el:List => new HaskellType(s"[${typeConverter(el.generic)}]")
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Haskell] = {
    val subs = subExpressions(exp)
    val zero = "0.0"
    val one = "1.0"
    val negOne = "(0 -1.0)"    // Haskell has problems with unary neg

    // generate the actual body
    op match {
      case Collect =>

        exp match {
          case Lit => Seq(Haskell("[a1]"))
          case Neg => Seq(Haskell("a1"))

          case Add | Sub | Mult | Divd => Seq(Haskell("a1 ++ a2"))

        }
          // Simplify only works for solutions that instantiate expression instances
      case Simplify  =>

        exp match {
          case Lit => Seq(inst(Lit)(op)(standardArgs(Lit)))
          case Neg => Seq(Haskell(s"""|
                              |    let left = eval a1
                              |    in if (left == 0)
                              |       then ${inst(Lit)(op)(zero)}
                              |       else ${inst(Neg)(op)(standardArgs(Neg))}
                              |""".stripMargin))

          case Add => Seq(Haskell(s"""|
                               |    let left = eval a1
                               |        right = eval a2
                               |    in if ((left == 0 && right == 0.0) || (left + right == 0.0))
                               |        then ${inst(Lit)(op)(zero)}
                               |        else if (left == 0)
                               |             then a2
                               |             else if (right == 0)
                               |                  then a1
                               |                  else ${inst(Add)(op)(standardArgs(Add))}
                               |""".stripMargin))

          case Sub => Seq(Haskell(s"""|
                              |    let left = eval a1
                              |        right = eval a2
                              |    in if (left == right)
                              |        then ${inst(Lit)(op)(zero)}
                              |        else ${inst(Sub)(op)(standardArgs(Add))}
                              |""".stripMargin))

          case Mult => Seq(Haskell(s"""|
                                |    let left = eval a1
                                |        right = eval a2
                                |    in if (left == 0 || right == 0.0)
                                |        then ${inst(Lit)(op)(zero)}
                                |        else if (left == 1)
                                |             then a2
                                |             else if (right == 1)
                                |                  then a1
                                |                  else ${inst(Mult)(op)(standardArgs(Add))}
                                |""".stripMargin))

          case Divd => Seq(Haskell(s"""|
                                |    let left = eval a1
                                |        right = eval a2
                                |    in if (left == 0)
                                |        then ${inst(Lit)(op)(zero)}
                                |        else if (right == 1)
                                |             then a1
                                |             else if (left == right)
                                |                  then ${inst(Lit)(op)(one)}
                                |                  else if (left == (0 - right))
                                |                       then ${inst(Lit)(op)(negOne)}
                                |                       else ${inst(Mult)(op)(standardArgs(Add))}
                                |""".stripMargin))

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {

    // (5/7) / (7-(2*3) --> just (5/7)
    val n1 = new domain.UnaryInst(Neg, new LitInst(5.0))
    val m1 = new domain.BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
    val m2 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, new LitInst(5.0),  new LitInst(2.0)), new LitInst(4.0))

    val d1 = new domain.BinaryInst(Divd, new LitInst(5.0), new LitInst(7.0))
    val s1 = new domain.BinaryInst(Sub, new LitInst(7.0), m1)
    val d2 = new domain.BinaryInst(Divd, d1, s1)

    val exp_n1:String = expand("n1_", n1).map(line => s"$line :: GeneralExpr").mkString("\n")
    val exp_m2:String = expand("m2_", m2).map(line => s"$line :: GeneralExpr").mkString("\n")
    val exp_d1:String = expand("d1_", d2).map(line => s"$line :: GeneralExpr").mkString("\n")
    val exp_d2:String = expand("d2_", d2).map(line => s"$line :: GeneralExpr").mkString("\n")

    super.testGenerator :+ new Haskell(
      s"""
         |$exp_n1
         |$exp_m2
         |$exp_d2
         |$exp_d1
         |test_e4_1 = TestCase (assertEqual "NegCheck-Eval" (0-5.0) (Eval.eval n1_))
         |test_e4_2 = TestCase (assertEqual "Simplify-Print" (Print.print d1_) (Print.print d2_))
         |-- collect test case
         |test_e4_3 = TestCase (assertEqual "Collect-D1" [5,7,7,2,3] (Collect.collect d2_))
         |
         |test_e4 = TestList [ TestLabel "1" test_e4_1, TestLabel "2" test_e4_2, TestLabel "3" test_e4_3 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e4
         |""".stripMargin)
  }

}
