package example.expression.haskell       /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends Evolution with HaskellGenerator with HUnitTestGenerator with Producer with M0 with M1 with M2 with M3 with M4 {
  self:e0 with e1 with e2 with e3 =>
  val domain:MathDomain
  import domain._

  /**
    * List can be accommodated (in Haskell) as a [a,b,c,d,e]
    */
  override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
    test.expect._1 match {   // was op.returnType.get
      case list:List =>
        val seq: Seq[Any] = test.expect._2.asInstanceOf[Seq[Any]]
        continue(new Haskell("[" + seq.mkString(",") + "]"))

      case _ => super.expected(test,id)(continue)
    }
  }

  /** If any new imports are needed for an operation, just extend here. */
  override def addedImports(op:domain.Operation):Seq[Haskell] = {
    op match {
      case Simplify => Seq(Haskell("import Eval"))
      case _ => super.addedImports(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : HaskellType = {
    tpe match {
      case el:List => new HaskellType(s"[${typeConverter(el.generic)}]")
      case _ => super.typeConverter(tpe)
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
          case Lit => result(Haskell(s"[${atts(litValue)}]"))
          case Neg => result(Haskell(s"${dispatch(atts(base.inner), op)}"))

          case Add | Sub | Mult | Divd => result(Haskell(s"${dispatch(atts(base.left), op)} ++ ${dispatch(atts(base.right), op)}"))

        }
          // Simplify only works for solutions that instantiate expression instances
      case Simplify  =>

        exp match {
          case Lit => Seq(inst(Lit)(op)(atts(litValue)))   // standardArgs(Lit)
          case Neg => Seq(Haskell(s"""|
                              |    let leftVal = ${dispatch(atts(base.inner), Eval)}
                              |    in if leftVal == 0
                              |       then ${result(inst(Lit)(op)(zero)).mkString("\n")}
                              |       else ${result(inst(Neg)(op)(standardVarArgs(Neg) : _*)).mkString("\n")}
                              |""".stripMargin))

          case Add => Seq(Haskell(s"""|
                               |    let leftVal = ${Eval.name} ${dispatch(atts(base.left), op)}
                               |        rightVal = ${Eval.name} ${dispatch(atts(base.right), op)}
                               |    in if (leftVal == 0 && rightVal == 0.0) || (leftVal + rightVal == 0.0)
                               |        then ${result(inst(Lit)(op)(zero)).mkString("\n")}
                               |        else if leftVal == 0
                               |             then ${result(dispatch(atts(base.right), op)).mkString("\n")}
                               |             else if rightVal == 0
                               |                  then ${result(dispatch(atts(base.left), op)).mkString("\n")}
                               |                  else ${result(inst(Add)(op)(standardVarArgs(Add) : _*)).mkString("\n")}
                               |""".stripMargin))

          case Sub => Seq(Haskell(s"""|
                              |    let leftVal = eval ${dispatch(atts(base.left), op)}
                              |        rightVal = eval ${dispatch(atts(base.right), op)}
                              |    in if leftVal == rightVal
                              |        then ${result(inst(Lit)(op)(zero)).mkString("\n")}
                              |        else ${result(inst(Sub)(op)(standardVarArgs(Add) : _*)).mkString("\n")}
                              |""".stripMargin))

          case Mult => Seq(Haskell(s"""|
                                |    let leftVal = eval ${dispatch(atts(base.left), op)}
                                |        rightVal= eval ${dispatch(atts(base.right), op)}
                                |    in if leftVal == 0 || rightVal == 0.0
                                |        then ${result(inst(Lit)(op)(zero)).mkString("\n")}
                                |        else if leftVal == 1
                                |             then ${result(dispatch(atts(base.right), op)).mkString("\n")}
                                |             else if rightVal == 1
                                |                  then ${result(dispatch(atts(base.left), op)).mkString("\n")}
                                |                  else ${result(inst(Mult)(op)(standardVarArgs(Add) : _*)).mkString("\n")}
                                |""".stripMargin))

          case Divd => Seq(Haskell(s"""|
                                |    let leftVal = eval ${dispatch(atts(base.left), op)}
                                |        rightVal = eval ${dispatch(atts(base.right), op)}
                                |    in if leftVal == 0
                                |        then ${result(inst(Lit)(op)(zero)).mkString("\n")}
                                |        else if rightVal == 1
                                |             then ${result(dispatch(atts(base.left), op)).mkString("\n")}
                                |             else if leftVal == rightVal
                                |                  then ${result(inst(Lit)(op)(one)).mkString("\n")}
                                |                  else if leftVal == (0 - rightVal)
                                |                       then ${result(inst(Lit)(op)(negOne)).mkString("\n")}
                                |                       else ${result(inst(Mult)(op)(standardVarArgs(Add) : _*)).mkString("\n")}
                                |""".stripMargin))

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    super.testGenerator :+ hunitMethod(M4_tests)
  }

}
