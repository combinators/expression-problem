package org.combinators.ep.language.cpp.visitorTable    /*DI:LD:AD*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.cpp._

trait CPPTableTestGenerator extends CPPGenerator with TestGenerator {

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
    new CPPExpression(s"(new ${op.concept}(${rec_convert(inst)}))->getValue()")
  }

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def rec_convert(inst: Inst): CPPExpression = {
    val name = inst.name
    vars(inst)   // cause the creation of a mapping to this instance
    id = id + 1
    inst match {
      case ui: UnaryInst =>
        val inner = rec_convert(ui.inner).toString
        new CPPExpression(s"new ${ui.e.concept}($inner)")

      case bi: BinaryInst =>
        val left = rec_convert(bi.left).toString
        val right = rec_convert(bi.right).toString
        new CPPExpression(s"new ${bi.e.concept}($left, $right)")

      //  double val1 = 1.0;
      //  Lit  lit1 = Lit(&val1);
      case exp: AtomicInst => new CPPExpression(s"new ${exp.e.concept}(${exp.ei.inst})")
      case _ => new CPPExpression(s""" "unknown $name" """)
    }
  }

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def rec_convertOLD(inst: Inst): CPPElement = {
    val name = inst.name
    id = id + 1
    inst match {
      case ui: UnaryInst =>
        val inner = rec_convert(ui.inner).toString
        new CPPStatement(s"$inner\n$name ${vars(inst)} = $name(&${vars(ui.inner)});")

      // Add  add3 = Add(&lit1, &lit2);
      case bi: BinaryInst =>
        val left = rec_convert(bi.left).toString
        val right = rec_convert(bi.right).toString
        new CPPStatement(s"$left\n$right\n$name ${vars(inst)} = $name(&${vars(bi.left)}, &${vars(bi.right)});")

      //  double val1 = 1.0;
      //  Lit  lit1 = Lit(&val1);
      case exp: AtomicInst =>
        new CPPStatement(
          s"""
             |double val${vars(inst)} = ${exp.ei.inst};
             |$name ${vars(inst)} = $name(&val${vars(inst)});
         """.stripMargin)

      case _ => new CPPStatement(s""" "unknown $name" """)
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
