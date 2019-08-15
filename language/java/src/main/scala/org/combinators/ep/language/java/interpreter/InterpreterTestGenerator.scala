package org.combinators.ep.language.java.interpreter   /*DI:LD:AD*/

import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.generator.LanguageIndependentTestGenerator
import org.combinators.ep.language.java.{JUnitTestGenerator, JavaGenerator}
import org.combinators.templating.twirl.Java

/**
  * Interpreter needs to know the last model with operations for the given vertex in the extension graph.
  *
  * Note that we need LitInst for our test generation, so we just grab from M0
  */
trait InterpreterTestGenerator
  extends JUnitTestGenerator
    with JavaGenerator
    with LanguageIndependentTestGenerator {
  self: InterpreterGenerator =>
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Interpreter needs a function to get the active model. */
  def getModel:domain.Model

  abstract override def testTypeConverter(typeRep: TypeRep) : Type = {
    if (typeRep == baseTypeRep) {
      Java(baseInterfaceName(getModel)).tpe()
    } else {
      super.testTypeConverter(typeRep)
    }
  }

//  def findHighest(inst:domain.Inst) : Model = {
//    inst match {
//      case n:NaryInst =>
//
//        val here = n.e match {
//          case a:Atomic =>
//            getModel.findType(a)
//          case b:Binary =>
//            getModel.findType(b)
//          case u:Unary =>
//            getModel.findType(u)
//        }
//
//        val next = n.instances.foldLeft(emptyModel()) { case (highest, i2) =>
//            val last = findHighest(i2)
//            if (highest.before(last)) { last } else { highest }
//        }
//
//        if (here.before(next)) { next } else { here }
//
//      case b:BinaryInst =>
//        val here:Model = getModel.findType(b.e)
//        val nextLeft:Model = findHighest(b.left)
//        val nextRight:Model = findHighest(b.right)
//        val next = if (nextLeft.before(nextRight)) { nextRight } else { nextLeft }
//        if (here.before(next)) { next } else { here }
//
//      case u:UnaryInst => {
//        val here:Model = getModel.findType(u.e)
//        val next:Model = findHighest(u.inner)
//        if (here.before(next)) { next } else { here }
//      }
//
//      case a:AtomicInst => getModel.findType(a.e)
//    }
//  }

//  /**
//    * Actual value in a test case.
//    *
//    * Each basic test case has an instance over which an operation is to be performed. This method
//    * returns the inline expression resulting from dispatching operation, op, over the given instance, inst.
//    *
//    * If the op is a producer operation, and any exp in the inst comes after it is defined, then
//    * we need to LIFT up to the highest level
//    */
//  override def actual(op: domain.Operation, inst: domain.Inst, params: Expression*): CodeBlockWithResultingExpressions = {
//    op match {
//      case po:domain.ProducerOperation =>
//        val highest = findHighest(inst)
//        val opdef = getModel.findOperation(op)
//        if (opdef.before(highest)) {
//          // must define interjection here.
//          val result = toTargetLanguage(inst).appendDependent(instExp =>
//            CodeBlockWithResultingExpressions(contextDispatch(NoSource, dispatchToExpression(instExp.head, op, params: _*)))
//          )
//          result
//        } else {
//          super.actual(op, inst, params : _*)
//        }
//      case _ => super.actual(op, inst, params : _*)
//    }
//  }

  /**
    * Consequential composite test cases might have producer operations that need to be
    * handled specially.
    *
    * @param test
    * @param idx
    * @return
    */
  override def junitTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
    test match {

      case seq: EqualsCompositeTestCase =>
        val expectedBlock = toTargetLanguage(seq.expect)

        val actualStartBlock = {
          val parameterBlock =
            seq.ops.head._2.foldLeft(CodeBlockWithResultingExpressions.empty) {
              case (b, p) => b.appendIndependent(toTargetLanguage(p))
            }

          parameterBlock.appendDependent(params => {
            val firstOp = seq.ops.head._1
            firstOp match {
              case po: domain.ProducerOperation => {
                val produced = actual(firstOp, seq.inst, params: _*)

                // at this point we need to add the conversion up
                val highest = getModel.lastModelWithOperation()
                val highestOps: String = highest.ops.sortWith(_.name < _.name).map(op => op.concept).mkString("")
                val defined = getModel.findOperation(firstOp)
                val definedOps: String = defined.ops.sortWith(_.name < _.name).map(op => op.concept).mkString("")

                if (definedOps.equals(highestOps)) {
                  actual(firstOp, seq.inst, params: _*)
                } else {
                  val pastBody: String = produced.resultingExpressions.mkString("\n")
                  val expr2 = s"$pastBody.accept(new ${definedOps}ExpTo$highestOps${domain.baseTypeRep.concept}Factory())"
                  CodeBlockWithResultingExpressions(
                    Java(expr2).expression()
                  )
                }
              }
                case _ => actual(firstOp, seq.inst, params: _*)
              }
            }
          )
        }

        val actualBlock = seq.ops.tail.foldLeft(actualStartBlock) { case (currentBlock, (nextOp, nextParams)) =>
          currentBlock.appendDependent { case Seq(currentResult) =>
            val parameterBlock =
              nextParams.foldLeft(CodeBlockWithResultingExpressions.empty) {
                case (b, p) => b.appendIndependent(toTargetLanguage(p))
              }
            parameterBlock.appendDependent(params =>
              nextOp match {
                case pm:domain.ProducerOperation =>

                  val highest = getModel.lastModelWithOperation()
                  val highestOps:String = highest.ops.sortWith(_.name < _.name).map(op => op.concept).mkString("")
                  val defined = getModel.findOperation(nextOp)
                  val definedOps:String = defined.ops.sortWith(_.name < _.name).map(op => op.concept).mkString("")

                  // any producer operation must be lifted to final one
                  val expr1 = contextDispatch(NoSource, dispatchToExpression(currentResult, nextOp, params: _*))
                  CodeBlockWithResultingExpressions(
                    Java(s"$expr1.accept(new ${definedOps}${domain.baseTypeRep.concept}To${highest}${domain.baseTypeRep.concept}Factory())").expression()
                  )

                case _ =>
                  CodeBlockWithResultingExpressions(
                    contextDispatch(NoSource, dispatchToExpression(currentResult, nextOp, params: _*))
                  )
              }
            )
          }
        }

        expectedBlock.appendDependent { case Seq(expectedValue) =>
          actualBlock.appendDependent { case Seq(actualValue) =>
            CodeBlockWithResultingExpressions(Java(s"assertEquals($expectedValue, $actualValue); ").statement())()
          }
        }.block

      case _ => super.junitTestMethod(test, idx)
    }
  }

  /** We need to import the static factory methods for the latest model with an operation */
  abstract override def generateSuite(pkg: Option[String]): Seq[CompilationUnit] = {
    val latestModelWithOp = getModel.lastModelWithOperation()
    val factoryClassName: String = {
      val classify = latestModelWithOp.ops.sortWith(_.name < _.name).map(op => op.concept).mkString("")
      s"interpreter.$classify${baseTypeRep.concept}Factory"
    }

    val suite = super.generateSuite(pkg)

    // these are static imports
    suite.foreach { compilationUnit =>
      compilationUnit.addImport(factoryClassName, true, true)
    }

    suite
  }
}
