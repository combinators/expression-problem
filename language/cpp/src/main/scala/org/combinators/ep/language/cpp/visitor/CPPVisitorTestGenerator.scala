package org.combinators.ep.language.cpp.visitor    /*DI:LD:AD*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.cpp._

trait CPPVisitorTestGenerator extends CPPGenerator with CPPUnitTestGenerator {

  val domain: BaseDomain with ModelDomain
  import domain._

  /**
    * Instantiating an expression invokes 'new'.
    *
    * @param exp       desired DataType subtype
    * @param params    potential parameters
    * @return
    */
  override def inst(exp:domain.DataType, params:Expression*): CodeBlockWithResultingExpressions = {
    val args = params.mkString(",")
    CodeBlockWithResultingExpressions(new CPPExpression(s"new ${exp.concept}($args)"))
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[CPPFile] = {

    val builders:Seq[String] = getModel.flatten().types.map(exp => {
      val list = exp.attributes
        .map(att => {

         val tpe = att.tpe match {
            case  domain.baseTypeRep => typeConverter(att.tpe)
            case _ => typeConverter(att.tpe)
          }
          s"$tpe ${att.instance}"
        }).mkString(",")
      val params = exp.attributes
        .map(att => att.instance).mkString(",")

      s"${exp.concept} *${exp.instance}($list) { return new ${exp.concept}($params); }"
    })

    val allOps = getModel.flatten().ops.map(op => s"""#include "${op.concept}.h" """)

    // add header files
    super.generateSuite(pkg, model).map(file =>
      file.addHeader(allOps).addHeader(builders))
  }
}
