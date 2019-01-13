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

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[CPPFile] = {

    val allOps = getModel.flatten().ops.map(op => s"""#include "${op.name.capitalize}.h" """)
    var num: Int = 0
    val files: Seq[CPPFile] = testGenerator.map(sa => {
      num = num + 1

      // standard imports
      sa.addHeader(Seq(
        """#include "CppUTest/TestHarness.h" """,
        """#include "CppUTest/SimpleString.h" """,
        """#include "CppUTest/PlatformSpecificFunctions.h" """,
        """#include "CppUTest/TestMemoryAllocator.h" """,
        """#include "CppUTest/MemoryLeakDetector.h" """,
        """#include "CppUTest/CommandLineTestRunner.h" """,

        """#include "Exp.h" """,
        """#include "IVisitor.h" """

//          std::shared_ptr<Add> add(std::shared_ptr<Expr> left, std::shared_ptr<Expr> right) { return std::make_shared<Add>(left, right); }
//          std::shared_ptr<Lit> lit(double value) { return std::make_shared<Lit>(value); }


      ) ++ allOps)
    })
    files
  }
}
