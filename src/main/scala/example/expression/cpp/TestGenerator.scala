package example.expression.cpp     /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator extends CPPGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  def getModel:domain.Model

  /** Return sample C++ test cases. */
  def testGenerator: Seq[StandAlone] = Seq.empty

  var id = 0
  var variables = collection.mutable.Map[AtomicInst, String]()

  /** Register an instance and get its variable identifier. */
  def vars(inst:AtomicInst) : String = {
    if (!variables.contains(inst)) {
      variables = variables + (inst -> s"${inst.e.name}$id")
      id = id + 1
    }

    variables(inst)
  }

  /**
    * Return properly formatted expected value as a code fragment.
    *
    * This method provides an essential capability that is required, namely, converting an existing
    * test case into a sequence of C++ code fragments. The return value is a form of a continuation,
    * that is, it is a function f(exp) => Seq[Statement] which allows us to chain together any number
    * of test cases.
    *
    * The expected value is a pair (TypeRep, Any) which relies on ability to call toString from a code
    * fragment (test.expect._2.toString).
    *
    * However, if you are dealing with more complicated code fragments (i.e., when the value is a list) then
    * you will have to override this method accordingly.
    */
  def expected(test:TestCaseExpectedValue, id:String) : (CPPElement => Seq[CPPElement]) => Seq[CPPElement] = continue => {
    continue(new CPPElement(test.expect._2.toString))
  }

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
    //${dispatch(atts(litValue),op)};"))
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

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def convert(inst: AtomicInst): CPPElement = {
    val name = inst.e.name
    id = id + 1
    inst match {
      case ui: UnaryInst =>
        new CPPElement(s"$name ${vars(inst)} = $name(&${vars(ui.inner)});")

        // Add  add3 = Add(&lit1, &lit2);
      case bi: BinaryInst =>
        new CPPElement(s"$name ${vars(inst)} = $name(&${vars(bi.left)}, &${vars(bi.right)});")

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

  def testMethod(tests:Seq[TestCase]) : Seq[Statement] = {

    val stmts: Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1
      val idx = pair._2

      val id: String = s"v$idx"

      test match {
        case eq: EqualsTestCase =>
          val OP:String = eq.op.name.capitalize

          // The expected method takes in a function that will be called by the expected method. Now, the expected
          // method will pass in the expression (which is expected) into this function, and it is the job of that
          // function to return the variable.
          expected(eq, id)(expectedExpr =>
            Seq(
              new CPPElement("{"),
              rec_convert(eq.inst),
              new CPPElement(s"$OP *e = new $OP();"),
              new CPPElement(s"${vars(eq.inst)}.Accept(e);"), // FOR NOW only works with visitor...
              new CPPElement(s"CHECK_COMPARE($expectedExpr, ==, e->getValue(${vars(eq.inst)}));"),
              new CPPElement("delete e;"),
              new CPPElement("}"))
          )

        case ne: NotEqualsTestCase =>
          val OP:String = ne.op.name.capitalize

          // The expected method takes in a function that will be called by the expected method. Now, the expected
          // method will pass in the expression (which is expected) into this function, and it is the job of that
          // function to return the variable.
          expected(ne, id)(expectedExpr =>
            Seq(
              new CPPElement("{"),
              rec_convert(ne.inst),
              new CPPElement(s"$OP *e = new $OP();"),
              new CPPElement(s"${vars(ne.inst)}.Accept(e);"),
              new CPPElement(s"CHECK_COMPARE($expectedExpr, !=, e->getValue(${vars(ne.inst)}));"),
              new CPPElement("delete e;"),
              new CPPElement("}"))
          )

        case seq: EqualsCompositeTestCase => {
          val x: Expression = actual(seq.ops.head, seq.inst) // HACK: Only works for two-deep
          val y: Expression = dispatch(x, seq.ops.tail.head)
          expected(seq, id)(expectedExpr => Seq(new CPPElement(s"CHECK_COMPARE($expectedExpr, ==, $y);")))
        }
      }
    })

    stmts
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[CPPFile] = {

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
      """#include "ExpVisitor.h" """
      ) ++ allOps)
    })
    files
  }
}