package example.expression.haskell       /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends Evolution with AbstractGenerator with TestGenerator with Producer with M0 with M1 with M2 with M3 with M4 {
  self:e0 with e1 with e2 with e3 =>
  val domain:MathDomain
  import domain._

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
    val atts = subExpressions(exp)
    val zero = Haskell("0.0")
    val one = Haskell("1.0")
    val negOne = Haskell("(0 -1.0)")    // Haskell has problems with unary neg

    // generate the actual body
    op match {
      case Collect =>

        exp match {
          case Lit => Seq(Haskell(s"[${atts(litValue)}]"))
          case Neg => Seq(Haskell(s"${dispatch(op, atts(base.inner))}"))

          case Add | Sub | Mult | Divd => Seq(Haskell(s"${dispatch(op, atts(base.left))} ++ ${dispatch(op, atts(base.right))}"))

        }
          // Simplify only works for solutions that instantiate expression instances
      case Simplify  =>

        exp match {
          case Lit => Seq(inst(Lit)(op)(atts(litValue)))   // standardArgs(Lit)
          case Neg => Seq(Haskell(s"""|
                              |    let leftVal = ${Eval.name} ${dispatch(op, atts(base.inner))}
                              |    in if leftVal == 0
                              |       then ${inst(Lit)(op)(zero)}
                              |       else ${inst(Neg)(op)(standardArgs(Neg))}
                              |""".stripMargin))

          case Add => Seq(Haskell(s"""|
                               |    let leftVal = ${Eval.name} ${dispatch(op, atts(base.left))}
                               |        rightVal = ${Eval.name} ${dispatch(op, atts(base.right))}
                               |    in if (leftVal == 0 && rightVal == 0.0) || (leftVal + rightVal == 0.0)
                               |        then ${inst(Lit)(op)(zero)}
                               |        else if leftVal == 0
                               |             then ${dispatch(op, atts(base.right))}
                               |             else if rightVal == 0
                               |                  then ${dispatch(op, atts(base.left))}
                               |                  else ${inst(Add)(op)(standardArgs(Add))}
                               |""".stripMargin))

          case Sub => Seq(Haskell(s"""|
                              |    let leftVal = eval ${dispatch(op, atts(base.left))}
                              |        rightVal = eval ${dispatch(op, atts(base.right))}
                              |    in if leftVal == rightVal
                              |        then ${inst(Lit)(op)(zero)}
                              |        else ${inst(Sub)(op)(standardArgs(Add))}
                              |""".stripMargin))

          case Mult => Seq(Haskell(s"""|
                                |    let leftVal = eval ${dispatch(op, atts(base.left))}
                                |        rightVal= eval ${dispatch(op, atts(base.right))}
                                |    in if leftVal == 0 || rightVal == 0.0
                                |        then ${inst(Lit)(op)(zero)}
                                |        else if leftVal == 1
                                |             then ${dispatch(op, atts(base.right))}
                                |             else if rightVal == 1
                                |                  then ${dispatch(op, atts(base.left))}
                                |                  else ${inst(Mult)(op)(standardArgs(Add))}
                                |""".stripMargin))

          case Divd => Seq(Haskell(s"""|
                                |    let leftVal = eval ${dispatch(op, atts(base.left))}
                                |        rightVal = eval ${dispatch(op, atts(base.right))}
                                |    in if leftVal == 0
                                |        then ${inst(Lit)(op)(zero)}
                                |        else if rightVal == 1
                                |             then ${dispatch(op, atts(base.left))}
                                |             else if leftVal == rightVal
                                |                  then ${inst(Lit)(op)(one)}
                                |                  else if leftVal == (0 - rightVal)
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

    super.testGenerator :+ new Haskell(
      s"""
         |n1 = ${convert(n1)}
         |m2 = ${convert(m2)}
         |d2 = ${convert(d2)}
         |d1 = ${convert(d1)}
         |test_e4_1 = TestCase (assertEqual "NegCheck-Eval" (0-5.0) (${Eval.name} n1))
         |test_e4_2 = TestCase (assertEqual "Simplify-Print" (${PrettyP.name}  d1) (${PrettyP.name}  d2))
         |-- collect test case
         |test_e4_3 = TestCase (assertEqual "Collect-D1" [5,7,7,2,3] (${Collect.name} d2))
         |
         |test_e4 = TestList [ TestLabel "1" test_e4_1, TestLabel "2" test_e4_2, TestLabel "3" test_e4_3 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e4
         |""".stripMargin)
  }

}
