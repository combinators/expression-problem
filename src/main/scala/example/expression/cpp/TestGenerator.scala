package example.expression.cpp     /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator {
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