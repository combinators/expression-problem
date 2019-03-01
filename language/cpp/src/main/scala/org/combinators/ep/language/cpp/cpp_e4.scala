package org.combinators.ep.language.cpp    /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e4 extends Evolution with CPPGenerator with TestGenerator with CPPProducer with M0 with M1 with M2 with M3 with M4 {
  self:cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 =>

  import domain._

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Simplify => scala.List[domain.Operation](PrettyP, Eval)
      case _ => super.dependency(op)
    }
  }

//  /**
//    * List can be accommodated (in C++) by populating vectors with values drawn from test case.
//    *
//    * Calls 'continue' with an expression (the result of the prior new statements) and just concatenates all statements
//    */
//  override def expected(test:domain.TestCaseExpectedValue, id:String) : (CPPElement => Seq[CPPElement]) => Seq[CPPElement] = continue => {
//    test.expect.tpe match {
//      case list:List[_] =>
//        val seq: Seq[Any] = test.expect.inst.asInstanceOf[Seq[Any]]
//        val ctype:CPPType = typeConverter(list)
//        //val inner:CPPType = typeConverter(list.generic)
//
//        val map = seq.map(elt => s"result$id.push_back($elt);")
//        val str = s"""
//                     |$ctype result$id;
//                     |${map.mkString("\n")}
//                     |${continue(new CPPExpression(s"result$id")).mkString("\n")}
//             """.stripMargin
//        str.split("\n").map(line => new CPPExpression(line))
//
//      case _ => super.expected(test,id)(continue)
//    }
//  }

  /** Provides fresh names for temporary list objects. */
  object ListNameGenerator {
    private var nextNumber: Int = 0
    def nextFreshListName(): CPPExpression = {
      val nextName = new CPPExpression(s"tmpList$nextNumber")
      nextNumber += 1
      nextName
    }
  }

  /** E4 Introduces Lists of values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.tpe match {
      case tpe: List[_] =>
        ei.inst match {
          case s:Seq[tpe.generic.scalaInstanceType] =>
            val listName = ListNameGenerator.nextFreshListName()
            val ctype:CPPType = typeConverter(tpe)
            val initBlock =
              CodeBlockWithResultingExpressions(
                new CPPStatement(s"$ctype $listName;")
              )(listName)

            s.foldLeft(initBlock) {
              case (block, nextElem) =>
                block.appendDependent { case Seq(constructedList) =>
                  toTargetLanguage(domain.ExistsInstance(tpe.generic)(nextElem)).appendDependent { case Seq(nextElemExpr) =>
                    CodeBlockWithResultingExpressions(
                      new CPPStatement(s"$constructedList.push_back($nextElemExpr);")
                    )(constructedList)
                  }
                }
            }
        }
      case _ => super.toTargetLanguage(ei)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : CPPType = {
    tpe match {
      case el:List[_] =>
        val tpe = typeConverter(el.generic)
        new CPPType(s"std::vector<$tpe>")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:DataType, op:Operation): Seq[CPPStatement] = {
    // generate the actual body
    val source = Source(exp,op)
    op match {
      case Collect =>
        val tpe = op.returnType.get match {
          case list:List[_] => typeConverter(list.generic)
        }
        exp match {
          case Lit => Seq(new CPPStatement(
            s"""
            |std::vector < $tpe > vec;
            |vec.push_back(${valueOf(expression(exp, litValue))});
            |${result(new CPPExpression("vec")).mkString("\n")};""".stripMargin))

          case Neg => Seq(new CPPStatement(
            s"""
               |std::vector<$tpe> vec;
               |std::vector<$tpe> expv = ${dispatch(expression(exp,base.inner),op)};
               |vec.insert(vec.end(), expv.begin(), expv.end());
               |${result(new CPPExpression("vec")).mkString("\n")};""".stripMargin))
          case Add|Sub|Mult|Divd => Seq(new CPPStatement(
              s"""std::vector< $tpe > vec;
                 |std::vector< $tpe > leftv = ${dispatch(expression(exp, base.left),op)};
                 |std::vector< $tpe > rightv = ${dispatch(expression(exp, base.right),op)};
                 |
                 |vec.insert(vec.end(), leftv.begin(), leftv.end());
                 |vec.insert(vec.end(), rightv.begin(), rightv.end());
                 |${result(new CPPExpression("vec")).mkString("\n")};""".stripMargin))

          case _ => super.logic(exp, op)
        }

      case Simplify =>
        val zero = new CPPExpression("0.0")
        val one = new CPPExpression("1.0")
        val negOne = new CPPExpression("-1.0")
        val zeroResultBlock =
          inst(Lit, zero).appendDependent { case Seq(zeroLit) =>
            CodeBlockWithResultingExpressions(result(zeroLit): _*)()
          }
        val oneResultBlock =
          inst(Lit, one).appendDependent { case Seq(zeroLit) =>
            CodeBlockWithResultingExpressions(result(zeroLit): _*)()
          }
        val negOneResultBlock =
          inst(Lit, negOne).appendDependent { case Seq(zeroLit) =>
            CodeBlockWithResultingExpressions(result(zeroLit): _*)()
          }
        exp match {
            // STILL has work to do...
//          case Lit =>
//            val value = new CPPElement(s"${valueOf(expression(exp, litValue))}")
//            Seq(new CPPElement(s"""${result(inst(Lit, value)).mkString("\n")} """))

          case Lit =>
              inst(Lit, valueOf(expression(exp, litValue))).appendDependent{ case Seq(litExp) => {
              CodeBlockWithResultingExpressions(result(litExp): _*)()
            }}.block

          case Add =>
            val deltaLeft = deltaChildOp(exp, domain.base.left, Eval)
            val deltaRight = deltaChildOp(exp, domain.base.right, Eval)

            val dispatchBothResultBlock =
              inst(Add,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(addResult) =>
                CodeBlockWithResultingExpressions(result(addResult): _*)()
              }
            Seq(new CPPStatement(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else if (leftVal == 0) {
                     |   ${result(dispatch(expression(exp, domain.base.right), Simplify)).mkString("\n")}
                     |} else if (rightVal == 0) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}""".stripMargin))

//          case Add =>
//            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
//            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
//            Seq(new CPPElement(s"""
//                      |double leftV = ${contextDispatch(source, deltaLeft)};
//                      |double rightV = ${contextDispatch(source, deltaRight)};
//                      |if (leftV + rightV == 0) {
//                      |  ${result(inst(Lit, zero)).mkString("\n")}
//                      |} else if (leftV == 0) {
//                      |  ${result(dispatch(expression(exp, domain.base.right), Simplify)).mkString("\n")}
//                      |} else if (rightV == 0) {
//                      |  ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
//                      |} else {
//                      |  ${result(inst(Add, dispatch(expression(exp, domain.base.left), Simplify),dispatch(expression(exp, domain.base.right), Simplify))).mkString("\n")}
//                      |}""".stripMargin))

          case Sub =>
            val deltaLeft = deltaChildOp(exp, domain.base.left, Eval)
            val deltaRight = deltaChildOp(exp, domain.base.right, Eval)
            val dispatchBothResultBlock =
              inst(Sub,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(addResult) =>
                CodeBlockWithResultingExpressions(result(addResult): _*)()
              }
            Seq(new CPPStatement(s"""|if (${contextDispatch(source, deltaLeft)} == ${contextDispatch(source, deltaRight)}) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}""".stripMargin))

//          case Sub =>
//            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
//            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
//            Seq(new CPPElement(s"""
//                      |double leftV = ${contextDispatch(source, deltaLeft)};
//                      |double rightV = ${contextDispatch(source, deltaRight)};
//                      |if (leftV == rightV) {
//                      |  ${result(inst(Lit, zero)).mkString("\n")}
//                      |} else {
//                      |  ${result(inst(Sub, dispatch(expression(exp, domain.base.left), Simplify),dispatch(expression(exp, domain.base.right), Simplify))).mkString("\n")}
//                      |}""".stripMargin))

          case Mult =>
            val deltaLeft = deltaChildOp(exp, domain.base.left, Eval)
            val deltaRight = deltaChildOp(exp, domain.base.right, Eval)
            val dispatchBothResultBlock =
              inst(Mult,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(addResult) =>
                CodeBlockWithResultingExpressions(result(addResult): _*)()
              }
            Seq(new CPPStatement(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if (leftVal == 0 || rightVal == 0) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else if (leftVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.right), Simplify)).mkString("\n")}
                     |} else if (rightVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}
                     |""".stripMargin))

//          case Mult =>
//            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
//            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
//            Seq(new CPPElement(s"""
//                      |double leftV = ${contextDispatch(source, deltaLeft)};
//                      |double rightV = ${contextDispatch(source, deltaRight)};
//                      |if (leftV == 0 || rightV == 0) {
//                      |  ${result(inst(Lit, zero)).mkString("\n")}
//                      |} else if (leftV == 1) {
//                      |  ${result(dispatch(expression(exp, domain.base.right), Simplify)).mkString("\n")}
//                      |} else if (rightV == 1) {
//                      |  ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
//                      |} else {
//                      |  ${result(inst(Mult, dispatch(expression(exp, domain.base.left), Simplify), dispatch(expression(exp, domain.base.right), Simplify))).mkString("\n")}
//                      |}""".stripMargin))

          case Divd =>
            val deltaLeft = deltaChildOp(exp, domain.base.left, Eval)
            val deltaRight = deltaChildOp(exp, domain.base.right, Eval)
            val dispatchBothResultBlock =
              inst(Divd,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(addResult) =>
                CodeBlockWithResultingExpressions(result(addResult): _*)()
              }
            Seq(new CPPStatement(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if (leftVal == 0) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else if (rightVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else if (leftVal == rightVal) {
                     |   ${oneResultBlock.block.mkString("\n")}
                     |} else if (leftVal == -rightVal) {
                     |   ${negOneResultBlock.block.mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}
                     |""".stripMargin))

//          case Divd =>
//            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
//            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
//            Seq(new CPPElement(s"""
//                      |double leftV = ${contextDispatch(source, deltaLeft)};
//                      |double rightV = ${contextDispatch(source, deltaRight)};
//                      |if (leftV == 0) {
//                      |  ${result(inst(Lit, zero)).mkString("\n")}
//                      |} else if (rightV == 1) {
//                      |  ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
//                      |} else if (leftV == rightV) {
//                      |   ${result(inst(Lit, one)).mkString("\n")}
//                      |} else if (leftV == -rightV) {
//                      |   ${result(inst(Lit, negOne)).mkString("\n")}
//                      |} else {
//                      |  ${result(inst(Divd, dispatch(expression(exp, domain.base.left), Simplify), dispatch(expression(exp, domain.base.right), Simplify))).mkString("\n")}
//                      |}""".stripMargin))

          case Neg =>
            val deltaInner = deltaChildOp(exp, domain.base.inner, Eval)
            val dispatchBothResultBlock =
              inst(Neg, dispatch(expression(exp, domain.base.inner), Simplify))
                .appendDependent{ case Seq(addResult) =>
                  CodeBlockWithResultingExpressions(result(addResult): _*)()
                }
            Seq(new CPPStatement(s"""
                    |if (${contextDispatch(source, deltaInner)} == 0) {
                    |   ${zeroResultBlock.block.mkString("\n")}
                    |} else {
                    |   ${dispatchBothResultBlock.block.mkString("\n")}
                    |}""".stripMargin))

          // TODO: Would love to have ability to simplify neg(neg(x)) to just be x. This requires a form
          // of inspection that might not be generalizable...
//          case Neg =>
//            val deltaInner = deltaChildOp(source, domain.base.inner, Eval)
//            Seq(new CPPElement(s"""
//                      |if (${contextDispatch(source, deltaInner)} == 0) {
//                      |   ${result(inst(Lit, zero)).mkString("\n")}
//                      |} else {
//                      |   ${result(inst(Neg, dispatch(expression(exp,domain.base.inner), Simplify))).mkString("\n")}
//                      |}""".stripMargin))


          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M4_tests)
  }
}
