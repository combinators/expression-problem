package org.combinators.ep.language.gj   /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator

/**
  * Isolate Performance tests
  */
trait PerformanceTestGenerator extends GJGenerator with LanguageIndependentTestGenerator with TestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  object PerformanceTestNameGenerator {
    private var nextNowVar = 0
    private var nextBestVar = 0
    private var nextCacheVar = 0

    def nextNow(): Expression = {
      val result = GJ(s"now$nextNowVar")
      nextNowVar += 1
      result
    }
    def nextBest(): Expression = {
      val result = GJ(s"best$nextBestVar")
      nextBestVar += 1
      result
    }
    def nextCache(): Expression = {
      val result = GJ(s"cache$nextCacheVar")
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
  abstract override def gjTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
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
                     GJStatement(s"${testTypeConverter(p.tpe)} $cacheLine = $pExp;")
                   )(cacheLine)
                 }
                 b.appendIndependent(pBlock)
             }
           parameterBlock.appendDependent(params => {
             val initialInstBlock =
               toTargetLanguage(perf.initialInst).appendDependent { case Seq(instExp) =>
                 CodeBlockWithResultingExpressions(
                   GJStatement(s"${testTypeConverter(baseTypeRep)} $initialInstanceCache = $instExp;")
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
                         GJStatement(s"${testTypeConverter(p.tpe)} $cacheLine = $pExp;")
                       )(cacheLine)
                     }
                     b.appendIndependent(pBlock)
                 }
               val nextCodeBlock =
                 nextParameterBlock.appendDependent(params => {
                   val nextInstBlock =
                     toTargetLanguage(nextInst).appendDependent { case Seq(instExp) =>
                       CodeBlockWithResultingExpressions(
                         GJStatement(s"${testTypeConverter(baseTypeRep)} $nextInstCache = $instExp;")
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
                     Seq(GJStatement(
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
                        """.stripMargin)) : _*
                   )()
               (nextExpNumber + 1, nextBlock.appendIndependent(lastBlock))
             }._2
           )
         performanceBlock.block

       case _ => super.gjTestMethod(test, idx)
     }
  }
}
