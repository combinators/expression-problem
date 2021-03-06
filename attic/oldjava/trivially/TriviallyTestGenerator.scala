package org.combinators.ep.language.java.trivially   /*DI:LD:AD*/

//import org.combinators.ep.domain.BaseDomain
//import org.combinators.ep.generator.DomainIndependentTestGenerator
import org.combinators.ep.language.java.{JUnitTestGenerator, DomainIndependentJavaGenerator}
import org.combinators.templating.twirl.Java

/**
  * Interpreter needs to know the last model with operations for the given vertex in the extension graph.
  *
  * Note that we need LitInst for our test generation, so we just grab from M0
  */
trait TriviallyTestGenerator extends JUnitTestGenerator {
//    with DomainIndependentJavaGenerator with DomainIndependentTestGenerator {
//  self: TriviallyGenerator =>
//
//  val domain: BaseDomain with ModelDomain
//  import domain._
//
//  /** Interpreter needs a function to get the active model. */
//  def getModel:domain.Model
//
//  abstract override def testTypeConverter(typeRep: TypeRep) : Type = {
//    if (typeRep == baseTypeRep) { Java("FinalI").tpe() }
//    else super.testTypeConverter(typeRep)
//  }
}
