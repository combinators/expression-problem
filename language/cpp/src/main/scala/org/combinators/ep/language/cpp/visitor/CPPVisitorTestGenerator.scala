package org.combinators.ep.language.cpp.visitor    /*DI:LD:AD*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.cpp._

trait CPPVisitorTestGenerator extends CPPGenerator with CPPUnitTestGenerator {

  val domain: BaseDomain with ModelDomain
  import domain._

  /**
    * Actual value in a test case.
    *
    * Each basic test case has an instance over which an operation is to be performed. This method
    * returns the inline expression resulting from dispatching operation, op, over the given instance, inst.
    *
    * For more complicated structures, as with lists for example, this method will need to be overridden.
    *
    * Not sure, yet, how to properly pass in variable parameters.
    */
  def actual(op:Operation, inst:AtomicInst, params:CPPElement*):CPPElement = {
    val expression = rec_convert(inst)
    new CPPExpression(s"(new ${op.concept}($expression))->getValue()")
  }

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def rec_convert(inst: Inst): CPPExpression = {
    vars(inst)   // cause the creation of a mapping to this instance
    id = id + 1
    inst match {
      case ui: UnaryInst =>
        val inner = rec_convert(ui.inner).toString
        new CPPExpression(s"${ui.e.instance}($inner)")

      case bi: BinaryInst =>
        val left = rec_convert(bi.left).toString
        val right = rec_convert(bi.right).toString
        new CPPExpression(s"${bi.e.instance}($left, $right)")

      case exp: AtomicInst => new CPPExpression(s"${exp.e.instance}(${exp.ei.inst})")
      case _ => new CPPExpression(s""" "unknown ${inst.name}" """)
    }
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
