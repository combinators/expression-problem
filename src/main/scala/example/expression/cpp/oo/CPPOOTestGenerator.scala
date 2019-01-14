package example.expression.cpp.oo     /*DI:LD:AD*/

import example.expression.cpp.{CPPElement, CPPFile, CPPGenerator, TestGenerator}
import example.expression.domain.{BaseDomain, ModelDomain}

trait CPPOOTestGenerator extends CPPGenerator with TestGenerator {

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
    val preceding = rec_convert(inst)
    val varName:String = variables(inst)
    dispatch(new CPPElement(varName), op, params: _*)
  }

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def rec_convert(inst: AtomicInst): CPPElement = {
    val name = inst.e.name
    id = id + 1
    inst match {
      case ui: UnaryInst =>
        val inner = rec_convert(ui.inner).toString
        new CPPElement(s"$inner\n$name ${vars(inst)} = $name(&${vars(ui.inner)});")

      // Add  add3 = Add(&lit1, &lit2);
      case bi: BinaryInst =>
        val left = rec_convert(bi.left).toString
        val right = rec_convert(bi.right).toString
        new CPPElement(s"$left\n$right\n$name ${vars(inst)} = $name(&${vars(bi.left)}, &${vars(bi.right)});")

      //  double val1 = 1.0;
      //  Lit  lit1 = Lit(&val1);
      case exp: AtomicInst =>
        new CPPElement(
          s"""
             |double val${vars(inst)} = ${exp.i.get};
             |$name ${vars(inst)} = $name(&val${vars(inst)});
         """.stripMargin)

      case _ => new CPPElement(s""" "unknown $name" """)
    }
  }


}
