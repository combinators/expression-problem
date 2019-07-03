package org.combinators.ep.language.cpp.oo     /*DI:LD:AD*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.cpp._

trait CPPOOTestGenerator extends CPPGenerator with CPPUnitTestGenerator {

  val domain: BaseDomain with ModelDomain

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String]): Seq[CPPFile] = {

    val builders:Seq[String] = getModel.flatten().types.map(exp => {
      val list = exp.attributes
        .map(att => {

          val tpe = att.tpe match {
            case  domain.baseTypeRep => typeConverter(att.tpe)
            case _ => typeConverter(att.tpe)
          }

         s"$tpe ${att.instance}"
        }).mkString(",")

      val params = exp.attributes.map(att => att.instance).mkString(",")

      s"${exp.concept} *${exp.instance}($list) { return new ${exp.concept}($params); }"
    })

    val allTypes = getModel.flatten().types.map(exp => s"""#include "${exp.concept}.h" """)

    // add header files
    super.generateSuite(pkg).map(file =>
      file.addHeader(allTypes).addHeader(builders))
  }

}
