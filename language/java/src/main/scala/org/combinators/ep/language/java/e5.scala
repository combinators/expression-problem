package org.combinators.ep.language.java    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.NameExpr
import org.combinators.ep.domain.math.{M0, M5}
import org.combinators.ep.domain.tree.{Leaf, Node}
import org.combinators.ep.domain.{Evolution, Leaf, Node, Tree}
import org.combinators.ep.generator.OperationDependency
import org.combinators.templating.twirl.Java

/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with DomainIndependentJavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  import domain._

  /** Provides fresh names for temporary list objects. */
  object TreeNameGenerator {
    private var nextNumber: Int = 0
    def nextName(prefix:String = "leaf"): NameExpr = {
      val nextName = Java(s"$prefix$nextNumber").nameExpression()
      nextNumber += 1
      nextName
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case domain.AsTree => scala.List[domain.Operation](Identifier)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.TreeType => Java(s"tree.Tree").tpe()      // package class goes here.
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * Output sequence of initialization statements in post-order traversal
    * @param tree
    * @return
    */
  def XXXXrecursiveExpansion(tree:Tree) : CodeBlockWithResultingExpressions = {
    if (tree.asLeaf().isDefined) {
      val leafName = TreeNameGenerator.nextName()
      val tempList = TreeNameGenerator.nextName()
      CodeBlockWithResultingExpressions(
        Java(s"java.util.ArrayList<tree.Tree> $tempList = new java.util.ArrayList<>();").statement()
        )(tempList).appendDependent{ case Seq(constructedList1) =>
        CodeBlockWithResultingExpressions(
          Java(s"$tempList.add(new tree.Leaf(${tree.asLeaf().get.value});").statement()
        )(leafName).appendDependent { case Seq(constructedList2) =>
          CodeBlockWithResultingExpressions(
            Java(s"tree.Node $leafName = new tree.Node($tempList, ${tree.asLeaf().get.value});").statement()
          )(leafName)
        }
      }
    } else {
      // must be recursive
      val node = tree.asNode().get
      val nodeName = TreeNameGenerator.nextName("node")
      val extendBlock =
        CodeBlockWithResultingExpressions(
          Java(s"java.util.ArrayList<tree.Tree> $nodeName = new java.util.ArrayList<>();").statement()
        )(nodeName)

      node.children.foldRight(extendBlock) {
        case (childNode, block) =>
          block.appendDependent { case Seq(constructedList) =>
            toTargetLanguage(domain.ExistsInstance(TreeType)(childNode)).appendDependent { case Seq(nextElemExpr) =>
              CodeBlockWithResultingExpressions(
                Java(s"$constructedList.add($nextElemExpr);").statement()
              )(constructedList)
            }
          }
      }

      //extendBlock
    }
  }

  /// TODO: FIX ME FIX ME FIX ME
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.tpe match {
      case TreeType =>
        ei.inst match {
          case node:Node =>
            val listName = TreeNameGenerator.nextName("list")
            val initBlock =
              CodeBlockWithResultingExpressions(
                Java(s"java.util.ArrayList<tree.Tree> $listName = new java.util.ArrayList<>();").statement()
              )(listName)

            val nodeName = TreeNameGenerator.nextName("node")

            // do in reverse order
            node.children.reverse.foldRight(initBlock) {
              case (nextElem, block) =>
               // nextElem is of (scala) type Tree
                block.appendDependent { case Seq(constructedList) =>
                  toTargetLanguage(domain.ExistsInstance(TreeType)(nextElem)).appendDependent { case Seq(nextElemExpr) =>
                    CodeBlockWithResultingExpressions(
                      Java(s"$constructedList.add($nextElemExpr);").statement()
                    )(constructedList)
                  }
                }
            }.appendDependent{ case Seq(constructedList) =>
                CodeBlockWithResultingExpressions(Java(s"tree.Node $nodeName = new tree.Node($listName, ${node.label});").statement())(nodeName)
            }

            // each leaf has value that must go BEFORE nodes. not properly sure how to address this right
            // now sine this is meant to be recursive not iterative
          case node:Leaf =>
            val leafName = TreeNameGenerator.nextName()
            val initBlock =
              CodeBlockWithResultingExpressions(
                Java(s"tree.Tree $leafName = new tree.Leaf(${node.value});").statement()
              )(leafName)
            initBlock
        }

      case _ => super.toTargetLanguage(ei)
    }
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    val source = Source(exp,op)
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree =>
        val atts = subExpressions(exp)

        exp match {
          case Lit =>
            val attParams = atts.map(att => att._2.toString).mkString(",")
            //val deltaSelf = dispatchSelf(Identifier)
            //val rhs = contextDispatch(source, deltaSelf)
            result(Java(s" new tree.Leaf($attParams) ").expression[Expression]())

          case Add|Sub|Mult|Divd|Neg =>
            val attParams = atts.map(att => att._2.toString + ".astree()").mkString(",")
            val deltaSelf = dispatchSelf(Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Java(s" new tree.Node(java.util.Arrays.asList($attParams), $rhs) ").expression[Expression]())
          }

        // moved here from m0
      case Identifier => result(Java(exp.hashCode.toString).expression())

      case _ => super.logic(exp, op)
    }
  }

  override def junitTestMethod(test:TestCase, idx:Int) : Seq[Statement] = {
      test match {
        case ctc: SameTestCase =>
            actual(AsTree, ctc.inst1).appendDependent { case Seq(treeLeft) =>
              actual(AsTree, ctc.inst2).appendDependent { case Seq(treeRight) =>
                val same = Java(s"$treeLeft.same($treeRight)").expression[Expression]()
                CodeBlockWithResultingExpressions(
                  if (ctc.result) {
                    Java(s"assertTrue($same);").statement()
                  } else {
                    Java(s"assertFalse($same);").statement()
                  }
                )()
              }
            }.block
        case _ => super.junitTestMethod(test, idx)
      }
    }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M5_tests)
  }
}
