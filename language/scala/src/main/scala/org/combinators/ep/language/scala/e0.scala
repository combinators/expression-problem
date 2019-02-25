package org.combinators.ep.language.scala   /*DD:LD:AI*/

import org.combinators.ep.domain.math.M0

import scala.meta.Stat

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends ScalaGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tr:TypeRep) : Type = {
    tr match {
      case Double => Scala("Double").tpe
      case Int => Scala("Int").tpe
      case _ => super.typeConverter(tr)
    }
  }

  /** E0 Introduces Double and Int values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case d:scala.Double => CodeBlockWithResultingExpressions(Scala(s"$d").expression)
      case i:scala.Int => CodeBlockWithResultingExpressions(Scala(s"$i").expression)
      case _ => super.toTargetLanguage(ei)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:DataType, op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(Scala(s"${expression(exp, litValue)}").expression)
          case Add => result(Scala(s"${dispatch(expression(exp, base.left),op)} + ${dispatch(expression(exp, base.right),op)}").expression)
          case _ => super.logic(exp, op)
        }

        // all future EXP sub-types can simply return hashcode.
      case Identifier => result(Scala(s"${exp.hashCode()}").expression)

      case _ => super.logic(exp, op)
    }
  }

  /**
    * Construct large trees and determine cost of evaluating over them.
    * @return
    */
 // abstract override def performanceMethod: Seq[Seq[Stat]] = {
    /*val a1 = new BinaryInst(Add, LitInst(1.0), LitInst(2.0))
    val numTrials = 10

    var trees = new BinaryInst(Add, a1, a1)
    var instantiations:String = s"val tree0  = ${toTargetLanguage(a1)}\n"
    var seq:String = s"val trees = Seq(tree0 "
    for (i <- 1 to numTrials) {
      instantiations = instantiations + s"val tree$i = ${convertRecursive(Add, s"tree${i-1}", s"tree${i-1}")}\n"
      trees = new BinaryInst(Add, trees, trees)
      seq = seq + s",tree$i"
    }
    seq = seq + ")"

    val source = NoSource
    val delta = deltaExprOp(source, new Scala("trees(i)").expression, Eval)
    val toTime = contextDispatch(source, delta)
    val evalPerfTest:Seq[Stat] = Scala(
      s"""
         |  $instantiations
         |  $seq
         |  for (i <- trees.length - 1 to 0 by -1) {
         |    var best = Long.MaxValue
         |    for (t <- 0 to 8) {
         |      val now = System.nanoTime()
         |      $toTime   // time this
         |      val duration = System.nanoTime() - now
         |      if (duration < best) {
         |        best = duration
         |      }
         |    }
         |    println(i + "," + best)
         |  }
         |""".stripMargin).statements

    val other = super.performanceMethod()
    other  :+ evalPerfTest
//    */
//
//    super.performanceMethod()
//  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M0_tests)
  }
}
