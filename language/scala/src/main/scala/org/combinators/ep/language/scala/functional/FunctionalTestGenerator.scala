package org.combinators.ep.language.scala.functional   /*DI:LD:AD*/

import org.combinators.ep.language.scala._

import scala.meta.Term

trait FunctionalTestGenerator extends TestGenerator {

  /** Actual value in a test case. */
//  override def actual(op:domain.Operation, inst:domain.Inst, terms:Term*):Expression = {
//    Scala (s"${op.instance}(${convert(inst)})").term
//  }

  override def actual(op: domain.Operation, inst: domain.Inst, params: Expression*): CodeBlockWithResultingExpressions = {
    toTargetLanguage(inst).appendDependent(instExp =>
      CodeBlockWithResultingExpressions(Scala (s"${op.instance}(${convert(inst)})").term)
    )
  }
}
