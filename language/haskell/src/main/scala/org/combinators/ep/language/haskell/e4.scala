package org.combinators.ep.language.haskell        /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._

/**
  * Truly independent of the specific design solution.
  */
trait e4 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 with M3 with M4 {
  self:e0 with e1 with e2 with e3 =>
  val domain:MathDomain
  import domain._

  /** E4 Introduces Lists of values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.tpe match {
      case tpe: List[_] =>
        ei.inst match {
          case seq:Seq[tpe.generic.scalaInstanceType] =>
          CodeBlockWithResultingExpressions(Haskell(seq.mkString("[", ",", "]")))
        }

      case _ => super.toTargetLanguage(ei)
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Simplify => scala.List[domain.Operation](Eval)
      case _ => super.dependency(op)
    }
  }

  // TODO: Shouldn't this be detected by dependency?
  /** If any new imports are needed for an operation, just extend here. */
  override def addedImports(op:domain.Operation):Seq[Haskell] = {
    op match {
      case Simplify => Seq(Haskell("import Eval"))
      case _ => super.addedImports(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : HaskellType = {
    tpe match {
      case el:List[_] => new HaskellType(s"[${typeConverter(el.generic)}]")
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[HaskellStatement] = {
    val source = Source (exp, op)
    val zero = Haskell("0.0")
    val one = Haskell("1.0")
    val negOne = Haskell("(0 -1.0)")    // Haskell has problems with unary neg
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

    // generate the actual body
    op match {
      case Collect =>

        exp match {
          case Lit => result(Haskell(s"[${expression(exp,litValue)}]"))
          case Neg => result(Haskell(s"${dispatch(expression(exp,base.inner), op)}"))
          case Add | Sub | Mult | Divd => result(Haskell(s"${dispatch(expression(exp,base.left), op)} ++ ${dispatch(expression(exp,base.right), op)}"))
        }

      case Simplify  =>

        // Haskell needs 'let' statements to get all variables properly assigned
        // may have multiple 'let' blocks. Some external adjustments are made
        // later based upon the need of the specific EP approach, due to the
        // nature of Haskell
        exp match {
          case Lit =>
            inst(Lit, expression(exp, litValue)).appendDependent{ case Seq(litExp) =>
              CodeBlockWithResultingExpressions(result(litExp):_*)()
            }.block

          case Neg =>
            val deltaInner = deltaChildOp(exp, domain.base.inner, Eval)
            val dispatchBothResultBlock =
              inst(Neg, dispatch(expression(exp, domain.base.inner), Simplify))
                .appendDependent{ case Seq(negResult) =>
                  CodeBlockWithResultingExpressions(result(negResult): _*)()
                }
            Seq(HaskellStatement(s"""|
                  |    let
                  |      leftVal = ${contextDispatch(source, deltaInner)}
                  |    in if leftVal == 0
                  |      then ${zeroResultBlock.block.mkString("\n")}
                  |      else ${dispatchBothResultBlock.block.mkString("\n")}
                  |""".stripMargin))

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
            Seq(HaskellStatement(s"""|
                |    let
                |      leftVal = ${contextDispatch(source, deltaLeft)}
                |      rightVal = ${contextDispatch(source, deltaRight)}
                |    in if (leftVal == 0 && rightVal == 0.0) || (leftVal + rightVal == 0.0)
                |      then ${zeroResultBlock.block.mkString("\n")}
                |      else if leftVal == 0
                |        then ${result(dispatch(expression(exp,base.right), op)).mkString("\n")}
                |        else if rightVal == 0
                |          then ${result(dispatch(expression(exp,base.left), op)).mkString("\n")}
                |          else ${dispatchBothResultBlock.block.mkString("\n")}
                |""".stripMargin))

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
            Seq(HaskellStatement(s"""|
                  |    let
                  |      leftVal = ${contextDispatch(source, deltaLeft)}
                  |      rightVal = ${contextDispatch(source, deltaRight)}
                  |    in if leftVal == rightVal
                  |      then ${zeroResultBlock.block.mkString("\n")}
                  |      else ${dispatchBothResultBlock.block.mkString("\n")}
                  |""".stripMargin))

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

            Seq(HaskellStatement(s"""|
                  |    let
                  |      leftVal = ${contextDispatch(source, deltaLeft)}
                  |      rightVal = ${contextDispatch(source, deltaRight)}
                  |    in if leftVal == 0 || rightVal == 0.0
                  |      then ${zeroResultBlock.block.mkString("\n")}
                  |      else if leftVal == 1
                  |        then ${result(dispatch(expression(exp,base.right), op)).mkString("\n")}
                  |        else if rightVal == 1
                  |          then ${result(dispatch(expression(exp,base.left), op)).mkString("\n")}
                  |          else ${dispatchBothResultBlock.block.mkString("\n")}
                  |""".stripMargin))

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

            Seq(HaskellStatement(s"""|
                  |    let
                  |      leftVal = ${contextDispatch(source, deltaLeft)}
                  |      rightVal = ${contextDispatch(source, deltaRight)}
                  |    in if leftVal == 0
                  |      then ${zeroResultBlock.block.mkString("\n")}
                  |      else if rightVal == 1
                  |        then ${result(dispatch(expression(exp,base.left), op)).mkString("\n")}
                  |        else if leftVal == rightVal
                  |          then ${oneResultBlock.block.mkString("\n")}
                  |          else if leftVal == (0 - rightVal)
                  |            then ${negOneResultBlock.block.mkString("\n")}
                  |            else ${dispatchBothResultBlock.block.mkString("\n")}
                  |""".stripMargin))

          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[UnitTest] = {
    super.testGenerator ++ testMethod(M4_tests) ++ testMethod(M4_simplify_tests)
  }
}
