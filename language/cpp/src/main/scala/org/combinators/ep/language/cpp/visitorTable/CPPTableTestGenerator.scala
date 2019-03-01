package org.combinators.ep.language.cpp.visitorTable    /*DI:LD:AD*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.cpp._

trait CPPTableTestGenerator extends CPPGenerator with CPPUnitTestGenerator {

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

  /** Converts types in test code. Need to use "Exp *" not just "Exp" */
  override def testTypeConverter(ty: TypeRep) : Type = {
    ty match {
      case domain.baseTypeRep => new CPPType(s"${typeConverter(ty)} *")
      case _ => super.typeConverter(ty)
    }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[CPPFile] = {

    val allOps = getModel.flatten().ops.map(op => s"""#include "${op.concept}.h" """)

    // add header files
    super.generateSuite(pkg, model).map(file =>
      file.addHeader(allOps))
  }
}
