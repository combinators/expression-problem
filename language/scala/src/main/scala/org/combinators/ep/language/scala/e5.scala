package org.combinators.ep.language.scala    /*DD:LD:AI*/

import org.combinators.ep.domain.math.{M0, M5, MathDomain}
import org.combinators.ep.domain.tree.Leaf
import org.combinators.ep.domain.{Evolution, Leaf, Node}
import org.combinators.ep.generator.OperationDependency

import scala.meta.{Stat, Type}

/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with ScalaGenerator with TestGenerator with OperationDependency with M0 with  M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case domain.AsTree => scala.List[domain.Operation](Identifier)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.TreeType => Type.Name("tree.Tree")      // package class goes here.
      case _ => super.typeConverter(tpe)
    }
  }

  /** Provides fresh names for temporary list objects. */
  object TreeNameGenerator {
    private var nextNumber: Int = 0
    def nextName(prefix:String = "leaf"): Expression = {
      val nextName = Scala(s"$prefix$nextNumber").expression
      nextNumber += 1
      nextName
    }
  }

  /// TODO: FIX ME FIX ME FIX ME
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.tpe match {
      case domain.TreeType =>
        ei.inst match {
          case node:Node =>
            val listName = TreeNameGenerator.nextName("list")
            val initBlock =
              CodeBlockWithResultingExpressions(
                Scala(s"var $listName:Seq[tree.Tree] = Seq.empty").statement
              )(listName)

            val nodeName = TreeNameGenerator.nextName("node")

            // do in reverse order
            node.children.reverse.foldRight(initBlock) {
              case (nextElem, block) =>
                // nextElem is of (scala) type Tree
                block.appendDependent { case Seq(constructedList) =>
                  toTargetLanguage(domain.ExistsInstance(domain.TreeType)(nextElem)).appendDependent { case Seq(nextElemExpr) =>
                    CodeBlockWithResultingExpressions(
                      Scala(s"$constructedList = $constructedList :+ $nextElemExpr").statement
                    )(constructedList)
                  }
                }
            }.appendDependent{ case Seq(constructedList) =>
              CodeBlockWithResultingExpressions(Scala(s"var $nodeName = new tree.Node($listName, ${node.label})").statement)(nodeName)
            }

          // each leaf has value that must go BEFORE nodes. not properly sure how to address this right
          // now sine this is meant to be recursive not iterative
          case node:Leaf =>
            val leafName = TreeNameGenerator.nextName()
            val initBlock =
              CodeBlockWithResultingExpressions(
                Scala(s"var $leafName = new tree.Leaf(${node.value})").statement
              )(leafName)
            initBlock
        }

      case _ => super.toTargetLanguage(ei)
    }
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    val source = Source(exp,op)
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree =>
        val atts = subExpressions(exp)

        exp match {
          case Lit =>
            val attParams = atts.map(att => att._2.toString).mkString(",")
            val deltaSelf = dispatchSelf(Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Scala(s"""new tree.Node(Seq(new tree.Leaf($attParams)), $rhs) """).expression)

          case Add|Sub|Mult|Divd|Neg =>
            val seq = atts.map(att => dispatch(att._2, domain.AsTree)).mkString(",")
            val deltaSelf = dispatchSelf(Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Scala(s"""new tree.Node(Seq($seq), $rhs ) """).expression)
          }

      // moved here from e0
      case Identifier => result(Scala(s"${exp.hashCode()}").expression)

      case _ => super.logic(exp, op)
    }
  }

  override def scalaTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
    test match {
      case ctc: SameTestCase =>

        actual(domain.AsTree, ctc.inst1).appendDependent { case Seq(treeLeft) =>
          actual(domain.AsTree, ctc.inst2).appendDependent { case Seq(treeRight) =>
            val same = Scala(s"$treeLeft.same($treeRight)").expression
            CodeBlockWithResultingExpressions(
              if (ctc.result) {
                Scala(s"assert(true == $same)").statement
              } else {
                Scala(s"assert(false == $same)").statement
              }
            )()
          }
        }.block
      case _ => super.scalaTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M5_tests)
  }
}
