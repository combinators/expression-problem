package example.expression.covariant.e4

import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import example.expression.covariant.InstanceCodeGenerators
import expression.Operation
import expression.data.{Add, Lit}
import expression.extensions._
import expression.instances.UnitTest
import expression.operations.SimplifyExpr
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator, HasTestCaseGenerator, OperationDependency}

import scala.collection.JavaConverters._

trait Model extends HasCodeGenerator with HasTestCaseGenerator with OperationDependency {

  // Get class that contains just PrettyP and SimplifyExp
  val subTypes:String = List(new PrettyP().getClass.getSimpleName,
    new SimplifyExpr().getClass.getSimpleName)
    .sortWith(_ < _)
    .mkString("")

  val simplifyGenerators = new SimplifyCodeGenerators(subTypes).simplifyGenerators

  /** Simplify depends upon having PrettyP. */
  abstract override def dependency(op:Operation): List[Operation] = {
    if (op.equals(new SimplifyExpr)) {
       super.dependency(op) :+ new PrettyP
    } else {
      super.dependency(op)
    }
  }

  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator

    val collectStatements:Seq[Statement] = Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                                |list.addAll(left().collectList());
                                                |list.addAll(right().collectList());
                                                |return list;
                                                |""".stripMargin).statements()

    // it is critical that the new changes are merged before old ones
    CodeGeneratorRegistry.merge(

      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Collect] {

        case (_, col:Collect) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(col).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),
            CodeGeneratorRegistry[Seq[Statement], Lit] {
              case (_, dataty:Lit) =>
                Java(
                  s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                      |list.add(value());
                      |return list;
                      |""".stripMargin).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Add] { case _ => collectStatements },
            CodeGeneratorRegistry[Seq[Statement], Sub] { case _ => collectStatements },
            CodeGeneratorRegistry[Seq[Statement], Mult] { case _ => collectStatements },
            CodeGeneratorRegistry[Seq[Statement], Divd] { case _ => collectStatements },
            CodeGeneratorRegistry[Seq[Statement], Neg] { case _ =>
              Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                      |list.addAll(exp().collectList());
                      |return list;
                      |""".stripMargin).statements()
            }
          )
      },

      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], SimplifyExpr] {
        case (_, simpl:SimplifyExpr) =>
          CodeGeneratorRegistry.merge(
            simplifyGenerators,
            oldGenerator(simpl).getOrElse(CodeGeneratorRegistry[Seq[Statement]])
          )
      },

      oldGenerator
    )
  }

  /**
    * Code generator for reproducing the structure of the covariant pattern invocation for collectLit.
    * Should be simplified since the repeated code exists
    */
  var collectLitGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.add(value());
                |return list;
                |""".stripMargin).statements()
    })

  val listStatements:Seq[Statement] = Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                              |list.addAll(left().collectList());
                                              |list.addAll(right().collectList());
                                              |return list;
                                              |""".stripMargin).statements()

  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Add] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Add) => listStatements })
  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Sub] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Sub) => listStatements })
  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Mult] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Mult) => listStatements })
  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Divd] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Divd) => listStatements })
  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Neg] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Neg) => Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                                                     |list.addAll(exp().collectList());
                                                                     |return list;
                                                                     |""".stripMargin).statements() })



  /**
    * Create test case code for eval where the expression "identifier"  has already been constructed
    * and the test case is UnitTest, which has its own expectations.
    *
    * Forms chain of responsibility
    */
  abstract override def testCaseGenerator(op:Operation, identifier:SimpleName, tc: UnitTest) : Seq[Statement] = {

    if (op.equals(new Collect)) {
      val num: Int = nextTestNumber()
      Java(s"""|  String result$num = (String) ${identifier.toString}.print();
               |  assertEquals("${tc.expected.toString}", result$num);
               |""".stripMargin).statements()

      var expected:String = s"java.util.List<Double> match$num = new java.util.ArrayList<Double>();"

      expected += tc.expected.asInstanceOf[java.util.List[expression.instances.Lit]].asScala.map(value => {
        s"match$num.add(" + value.value + ");"
      }).mkString("\n")

      Java(s"""|  java.util.List<Double> result$num = (java.util.List<Double>) ${identifier.toString}.collectList();
               |  $expected
               |  assertEquals(match$num, result$num);
               |""".stripMargin).statements()
    } else if (op.equals(new SimplifyExpr)) {
      val num: Int = nextTestNumber()
      val subTypes:String = "PrettyP"   // all we care about (for test case) is that we can print...
      val expectedCode: Option[com.github.javaparser.ast.expr.Expression] =
        new InstanceCodeGenerators(subTypes).instanceGenerators(tc.expected.asInstanceOf[expression.instances.Instance])

      if (expectedCode.isDefined) {
        val initExpected: String = s"""$subTypes expectedExp${identifier.toString} = ${expectedCode.get.toString};"""

        // TODO: Create an equal visitor for use instead of depending on prettyP
        val str: String =
          s"""
             |  $initExpected
             |  $subTypes result$num = ($subTypes) ${identifier.toString}.simplify();
             |  assertEquals(expectedExp${identifier.toString}.print(), result$num.print());
             |""".stripMargin
        Java(str).statements()
      }
      else {
        // skip for lack of anything better.
        Java("// skip${op.name}$testNumber(){}\n").statements()
      }
    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }
}
