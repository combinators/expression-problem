package org.combinators.ep.language.java      /*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.NameExpr
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator
import org.combinators.templating.twirl.Java

trait TestGenerator extends JavaGenerator with LanguageIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  object PerformanceTestNameGenerator {
    private var nextNowVar = 0
    private var nextBestVar = 0
    private var nextCacheVar = 0
    def nextNow(): NameExpr = {
      val result = Java(s"now$nextNowVar").nameExpression()
      nextNowVar += 1
      result
    }
    def nextBest(): NameExpr = {
      val result = Java(s"best$nextNowVar").nameExpression()
      nextNowVar += 1
      result
    }
    def nextCache(): NameExpr = {
      val result = Java(s"cache$nextNowVar").nameExpression()
      nextNowVar += 1
      result
    }
  }

  type UnitTest = MethodDeclaration /** Base concept for the representation of a single test case. */

  /** Return sample test cases as methods. */
  def testGenerator: Seq[MethodDeclaration] = Seq.empty

  case class CachedTyRep(underlyingType: TypeRep) extends TypeRep {
    type scalaInstanceType = CachedExp
  }
  case class CachedExp(cacheLine: Expression) extends Inst {
    val name = "cached"
  }

  abstract override def typeConverter(tpe: TypeRep): Type = {
    tpe match {
      case CachedTyRep(ty) => typeConverter(ty)
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def toTargetLanguage(instance: Inst): CodeBlockWithResultingExpressions = {
    instance match {
      case CachedExp(cacheLine) => CodeBlockWithResultingExpressions(cacheLine)
      case _ => super.toTargetLanguage(instance)
    }
  }

  /** Return MethodDeclaration associated with given test cases. */
  def junitTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
     test match {
       case eq: EqualsTestCase =>
         val expectedBlock = toTargetLanguage(eq.expect)
         val parameterBlock =
           eq.params.foldLeft(CodeBlockWithResultingExpressions.empty) {
             case (b, p) => b.appendIndependent(toTargetLanguage(p))
           }

         val actualBlock = parameterBlock.appendDependent(params =>
             actual(eq.op, eq.inst, params: _*)
         )

         expectedBlock.appendDependent { case Seq(expectedValue) =>
           actualBlock.appendDependent { case Seq(actualValue) =>
             CodeBlockWithResultingExpressions(Java(s"assertEquals($expectedValue, $actualValue);").statement())()
           }
         }.block

       case ne: NotEqualsTestCase =>
         val unExpectedBlock = toTargetLanguage(ne.expect)
         val parameterBlock =
           ne.params.foldLeft(CodeBlockWithResultingExpressions.empty) {
             case (b, p) => b.appendIndependent(toTargetLanguage(p))
           }
         val actualBlock =
           parameterBlock.appendDependent(params =>
             actual(ne.op, ne.inst, params: _*)
           )

         unExpectedBlock.appendDependent { case Seq(unExpectedValue) =>
           actualBlock.appendDependent { case Seq(actualValue) =>
             CodeBlockWithResultingExpressions(Java(s"assertNotEquals($unExpectedValue, $actualValue);").statement())()
           }
         }.block

       case seq: EqualsCompositeTestCase =>
         val expectedBlock = toTargetLanguage(seq.expect)
         val actualStartBlock = {
           val parameterBlock =
             seq.ops.head._2.foldLeft(CodeBlockWithResultingExpressions.empty) {
               case (b, p) => b.appendIndependent(toTargetLanguage(p))
             }
           parameterBlock.appendDependent(params =>
             actual(seq.ops.head._1, seq.inst, params: _*)
           )
         }
         val actualBlock = seq.ops.tail.foldLeft(actualStartBlock) { case (currentBlock, (nextOp, nextParams)) =>
           currentBlock.appendDependent { case Seq(currentResult) =>
             val parameterBlock =
               nextParams.foldLeft(CodeBlockWithResultingExpressions.empty) {
                 case (b, p) => b.appendIndependent(toTargetLanguage(p))
               }
             parameterBlock.appendDependent(params =>
               CodeBlockWithResultingExpressions(
                 contextDispatch(NoSource, deltaExprOp(NoSource, currentResult, nextOp, params: _*))
               )
             )
           }
         }

         expectedBlock.appendDependent { case Seq(expectedValue) =>
           actualBlock.appendDependent { case Seq(actualValue) =>
             CodeBlockWithResultingExpressions(Java(s"assertEquals($expectedValue, $actualValue);").statement())()
           }
         }.block
       case perf: PerformanceTestCase =>
         val initialParamsWithCache =
           perf.initialParams.map(param => (param, PerformanceTestNameGenerator.nextCache()))
         val initialInstanceCache = PerformanceTestNameGenerator.nextCache()

         val actualStartBlock = {
           val parameterBlock =
             initialParamsWithCache.foldLeft(CodeBlockWithResultingExpressions.empty) {
               case (b, (p, cacheLine)) =>
                 val pBlock = toTargetLanguage(p).appendDependent { case Seq(pExp) =>
                   CodeBlockWithResultingExpressions(
                     Java(s"${typeConverter(p.tpe)} $cacheLine = $pExp;").statement()
                   )(cacheLine)
                 }
                 b.appendIndependent(pBlock)
             }
           parameterBlock.appendDependent(params => {
             val initialInstBlock =
               toTargetLanguage(perf.initialInst).appendDependent { case Seq(instExp) =>
                 CodeBlockWithResultingExpressions(
                   Java(s"${typeConverter(baseTypeRep)} $initialInstanceCache = $instExp;").statement()
                 )(initialInstanceCache)
               }
             initialInstBlock.appendDependent { case Seq(instExp) =>
               CodeBlockWithResultingExpressions(contextDispatch(NoSource, deltaExprOp(NoSource, instExp, perf.op, params: _*)))
             }
           })
         }

         val iteratedBlock =
          (0 until perf.iterations)
            .foldLeft(
              (actualStartBlock,
                CachedExp(initialInstanceCache),
                initialParamsWithCache.map(pc => ExistsInstance(CachedTyRep(pc._1.tpe))(CachedExp(pc._2))))
            ) {
             case ((lastCodeBlock, lastInst, lastParams), currentIteration) =>
               val nextParamsWithCache = perf.stepParams(lastParams).map(param => (param, PerformanceTestNameGenerator.nextCache()))
               val nextInst = perf.stepInstance(lastInst)
               val nextInstCache = PerformanceTestNameGenerator.nextCache()

               val nextParameterBlock =
                 nextParamsWithCache.foldLeft(CodeBlockWithResultingExpressions.empty) {
                   case (b, (p, cacheLine)) =>
                     val pBlock = toTargetLanguage(p).appendDependent { case Seq(pExp) =>
                       CodeBlockWithResultingExpressions(
                         Java(s"${typeConverter(p.tpe)} $cacheLine = $pExp;").statement()
                       )(cacheLine)
                     }
                     b.appendIndependent(pBlock)
                 }
               val nextCodeBlock =
                 nextParameterBlock.appendDependent(params => {
                   val nextInstBlock =
                     toTargetLanguage(nextInst).appendDependent { case Seq(instExp) =>
                       CodeBlockWithResultingExpressions(
                         Java(s"${typeConverter(baseTypeRep)} $nextInstCache = $instExp;").statement()
                       )(nextInstCache)
                     }
                   nextInstBlock.appendDependent { case Seq(instExp) =>
                     CodeBlockWithResultingExpressions(contextDispatch(NoSource, deltaExprOp(NoSource, instExp, perf.op, params: _*)))
                   }
                 })
               (lastCodeBlock.appendIndependent(nextCodeBlock),
                 CachedExp(nextInstCache),
                 nextParamsWithCache.map(pc => ExistsInstance(CachedTyRep(pc._1.tpe))(CachedExp(pc._2))))
           }

         val performanceBlock =
           iteratedBlock._1.appendDependent(actualExpressions =>
             actualExpressions.foldLeft((0, CodeBlockWithResultingExpressions.empty)) {
               case ((nextExpNumber, lastBlock), nextExp) =>
                 val now = PerformanceTestNameGenerator.nextNow()
                 val best = PerformanceTestNameGenerator.nextBest()
                 val nextBlock =
                   CodeBlockWithResultingExpressions(
                     Java(
                       s"""
                          |long $now = System.nanoTime();
                          |$nextExp;
                          |long $best = System.nanoTime() - $now;
                          |for (int i = 1; i < ${perf.bestOf}; i++) {
                          |    $now = System.nanoTime();
                          |    $nextExp;
                          |    long duration = System.nanoTime() - $now;
                          |    if (duration < $best) { $best = duration; }
                          |}
                          |System.out.println($nextExpNumber + "," + $best);
                        """.stripMargin).statements() : _*
                   )()
               (nextExpNumber + 1, nextBlock.appendIndependent(lastBlock))
             }._2
           )
         performanceBlock.block
     }
  }
  /** Return MethodDeclaration associated with given test cases. */
  def testMethod(tests: Seq[TestCase]): Seq[MethodDeclaration] = {
    val (performanceTests, validityTests) =
      tests.partition {
        case _: PerformanceTestCase => true
        case _ => false
      }

    def methodFor(methodName: String, tests: Seq[TestCase]): Seq[MethodDeclaration] = {
      val stmts = tests.zipWithIndex.flatMap { case (test, idx) => junitTestMethod(test, idx) }
      if (stmts.isEmpty) Seq.empty
      else {
        Java(
          s"""|public void $methodName() {
              |   ${stmts.mkString("\n")}
              |}""".stripMargin).methodDeclarations
      }
    }

    methodFor("test", performanceTests) ++ methodFor("test", validityTests)


  }

}
