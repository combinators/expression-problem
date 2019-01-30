package example.expression.haskell       /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  */
trait e4 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 with M3 with M4 {
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

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Haskell] = {
    val source = Source (exp, op)
    val zero = Haskell("0.0")
    val one = Haskell("1.0")
    val negOne = Haskell("(0 -1.0)")    // Haskell has problems with unary neg

    // generate the actual body
    op match {
      case Collect =>

        exp match {
          case Lit => result(Haskell(s"[${expression(exp,litValue)}]"))
          case Neg => result(Haskell(s"${dispatch(expression(exp,base.inner), op)}"))

          case Add | Sub | Mult | Divd => result(Haskell(s"${dispatch(expression(exp,base.left), op)} ++ ${dispatch(expression(exp,base.right), op)}"))

        }
          // Simplify only works for solutions that instantiate expression instances
      case Simplify  =>

        exp match {
          case Lit => Seq(inst(Lit, expression(exp,litValue)))   // standardArgs(Lit)
          case Neg =>
            val deltaInner = deltaChildOp(source, domain.base.inner, Eval)
            Seq(Haskell(s"""|
                 |    let leftVal = ${contextDispatch(source, deltaInner)}
                 |    in if leftVal == 0
                 |       then ${result(inst(Lit, zero)).mkString("\n")}
                 |       else ${result(inst(Neg, standardVarArgs(Neg) : _*)).mkString("\n")}
                 |""".stripMargin))

          case Add =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Seq(Haskell(s"""|
                 |    let leftVal = ${contextDispatch(source, deltaLeft)}
                 |        rightVal = ${contextDispatch(source, deltaRight)}
                 |    in if (leftVal == 0 && rightVal == 0.0) || (leftVal + rightVal == 0.0)
                 |        then ${result(inst(Lit, zero)).mkString("\n")}
                 |        else if leftVal == 0
                 |             then ${result(dispatch(expression(exp,base.right), op)).mkString("\n")}
                 |             else if rightVal == 0
                 |                  then ${result(dispatch(expression(exp,base.left), op)).mkString("\n")}
                 |                  else ${result(inst(Add, standardVarArgs(Add) : _*)).mkString("\n")}
                 |""".stripMargin))

          case Sub =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Seq(Haskell(s"""|
                 |    let leftVal = ${contextDispatch(source, deltaLeft)}
                 |        rightVal = ${contextDispatch(source, deltaRight)}
                 |    in if leftVal == rightVal
                 |        then ${result(inst(Lit, zero)).mkString("\n")}
                 |        else ${result(inst(Sub, standardVarArgs(Add) : _*)).mkString("\n")}
                 |""".stripMargin))

          case Mult =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Seq(Haskell(s"""|
                 |    let leftVal = ${contextDispatch(source, deltaLeft)}
                 |        rightVal = ${contextDispatch(source, deltaRight)}
                 |    in if leftVal == 0 || rightVal == 0.0
                 |        then ${result(inst(Lit, zero)).mkString("\n")}
                 |        else if leftVal == 1
                 |             then ${result(dispatch(expression(exp,base.right), op)).mkString("\n")}
                 |             else if rightVal == 1
                 |                  then ${result(dispatch(expression(exp,base.left), op)).mkString("\n")}
                 |                  else ${result(inst(Mult, standardVarArgs(Add) : _*)).mkString("\n")}
                 |""".stripMargin))

          case Divd =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            Seq(Haskell(s"""|
                 |    let leftVal = ${contextDispatch(source, deltaLeft)}
                 |        rightVal = ${contextDispatch(source, deltaRight)}
                 |    in if leftVal == 0
                 |        then ${result(inst(Lit, zero)).mkString("\n")}
                 |        else if rightVal == 1
                 |             then ${result(dispatch(expression(exp,base.left), op)).mkString("\n")}
                 |             else if leftVal == rightVal
                 |                  then ${result(inst(Lit, one)).mkString("\n")}
                 |                  else if leftVal == (0 - rightVal)
                 |                       then ${result(inst(Lit, negOne)).mkString("\n")}
                 |                       else ${result(inst(Mult, standardVarArgs(Add) : _*)).mkString("\n")}
                 |""".stripMargin))

          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    super.testGenerator :+ hunitMethod(M4_tests)
  }

}
