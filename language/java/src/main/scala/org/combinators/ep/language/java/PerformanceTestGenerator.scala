package org.combinators.ep.language.java    /*DI:LD:AI*/

import com.github.javaparser.ast.expr.NameExpr
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator
import org.combinators.templating.twirl.Java

/**
  * Isolate Performance tests
  */
trait PerformanceTestGenerator extends JavaGenerator with LanguageIndependentTestGenerator with TestGenerator {
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
      val result = Java(s"best$nextBestVar").nameExpression()
      nextBestVar += 1
      result
    }
    def nextCache(): NameExpr = {
      val result = Java(s"cache$nextCacheVar").nameExpression()
      nextCacheVar += 1
      result
    }
  }

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
  abstract override def junitTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
     test match {

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
                     Java(s"${testTypeConverter(p.tpe)} $cacheLine = $pExp;").statement()
                   )(cacheLine)
                 }
                 b.appendIndependent(pBlock)
             }
           parameterBlock.appendDependent(params => {
             val initialInstBlock =
               toTargetLanguage(perf.initialInst).appendDependent { case Seq(instExp) =>
                 CodeBlockWithResultingExpressions(
                   Java(s"${testTypeConverter(baseTypeRep)} $initialInstanceCache = $instExp;").statement()
                 )(initialInstanceCache)
               }
             initialInstBlock.appendDependent { case Seq(instExp) =>
               CodeBlockWithResultingExpressions(contextDispatch(NoSource, deltaExprOp(instExp, perf.op, params: _*)))
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
                         Java(s"${testTypeConverter(p.tpe)} $cacheLine = $pExp;").statement()
                       )(cacheLine)
                     }
                     b.appendIndependent(pBlock)
                 }
               val nextCodeBlock =
                 nextParameterBlock.appendDependent(params => {
                   val nextInstBlock =
                     toTargetLanguage(nextInst).appendDependent { case Seq(instExp) =>
                       CodeBlockWithResultingExpressions(
                         Java(s"${testTypeConverter(baseTypeRep)} $nextInstCache = $instExp;").statement()
                       )(nextInstCache)
                     }
                   nextInstBlock.appendDependent { case Seq(instExp) =>
                     CodeBlockWithResultingExpressions(contextDispatch(NoSource, deltaExprOp(instExp, perf.op, params: _*)))
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

       case _ => super.junitTestMethod(test, idx)
     }
  }

}
