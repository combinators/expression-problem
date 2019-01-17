package example.expression.cpp.visitor    /*DI:LD:AD*/

import example.expression.cpp._
import example.expression.domain.{BaseDomain, ModelDomain}

trait CPPVisitorTestGenerator extends CPPGenerator with TestGenerator {

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
    new CPPElement(s"(new ${op.name.capitalize}($expression))->getValue()")
  }

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def rec_convert(inst: AtomicInst): CPPElement = {
    val name = inst.e.name
    vars(inst)   // cause the creation of a mapping to this instance
    id = id + 1
    inst match {
      case ui: UnaryInst =>
        val inner = rec_convert(ui.inner).toString
        new CPPElement(s"${ui.e.name.toLowerCase()}($inner)")

      case bi: BinaryInst =>
        val left = rec_convert(bi.left).toString
        val right = rec_convert(bi.right).toString
        new CPPElement(s"${bi.e.name.toLowerCase()}($left, $right)")

      //  double val1 = 1.0;
      //  Lit  lit1 = Lit(&val1);
      case exp: AtomicInst => new CPPElement(s"${exp.e.name.toLowerCase()}(${exp.i.get})")
      case _ => new CPPElement(s""" "unknown $name" """)
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
          new CPPElement(s"$tpe ${att.name}")
        }).mkString(",")
      val params = exp.attributes
        .map(att => new CPPElement(att.name)).mkString(",")

      s"${exp.name.capitalize} *${exp.name.toLowerCase()}($list) { return new ${exp.name.capitalize}($params); }"
    })

    val allOps = getModel.flatten().ops.map(op => s"""#include "${op.name.capitalize}.h" """)

    // add header files
    super.generateSuite(pkg, model).map(file =>
      file.addHeader(allOps).addHeader(builders))
  }
}
