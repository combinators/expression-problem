package org.combinators.ep.language.java.interpreter   /*DI:LD:AD*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
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
      val myModel = getModel
      //val mname = modelInterfaceName(myModel)
      val mname = baseInterfaceName(myModel)
      Java(mname).tpe()
    } else {
      super.testTypeConverter(typeRep)
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
