package example.expression.visitor.e4

import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import example.expression.visitor.InstanceCodeGenerators
import expression.Operation
import expression.data.{Add, Lit}
import expression.extensions._
import expression.instances.UnitTest
import expression.operations.SimplifyExpr
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator, HasTestCaseGenerator}

import scala.collection.JavaConverters._

trait Model extends InstanceCodeGenerators with HasCodeGenerator with HasTestCaseGenerator {

  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator

    val collectStatements: Seq[Statement] =
      Java(
        s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
            |list.addAll(e.getLeft().accept(this));
            |list.addAll(e.getRight().accept(this));
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
                      |list.add(e.getValue());
                      |return list;
                      |""".stripMargin).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Add] { case _ => collectStatements },
            CodeGeneratorRegistry[Seq[Statement], Sub] { case _ => collectStatements },
            CodeGeneratorRegistry[Seq[Statement], Mult] { case _ => collectStatements },
            CodeGeneratorRegistry[Seq[Statement], Divd] { case _ => collectStatements },
            CodeGeneratorRegistry[Seq[Statement], Neg] { case _ =>
              Java(
                s"""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                    |list.addAll(e.getExp().accept(this));
                    |return list;""".stripMargin).statements()
            }
          )
      },

      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], SimplifyExpr] {
        case (_, simpl:SimplifyExpr) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(simpl).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),
            CodeGeneratorRegistry[Seq[Statement], Lit] { case _ =>
              Java(s"""return e;""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Add] { case _ =>
              Java(s"""
                      |double leftVal = e.getLeft().accept(new Eval());
                      |double rightVal = e.getRight().accept(new Eval());
                      |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                      |  return new Lit(0.0);
                      |} else if (leftVal == 0) {
                      |  return e.getRight().accept(this);
                      |} else if (rightVal == 0) {
                      |  return e.getLeft().accept(this);
                      |} else {
                      |  return new Add(e.getLeft().accept(this), e.getRight().accept(this));
                      |}
                      |""".stripMargin).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Sub] { case _ =>
              Java(s"""
                      |if (e.getLeft().accept(new Eval()).equals(e.getRight().accept(new Eval()))) {
                      |  return new Lit(0.0);
                      |} else {
                      |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
                      |}
                      |""".stripMargin).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Neg] { case _ =>
              Java(
                s"""
                   |if (e.getExp().accept(new Eval()) == 0) {
                   |  return new Lit(0.0);
                   |} else {
                   |  return e;
                   |}""".stripMargin).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Mult] { case _ =>
              Java(
                s"""
                   |double leftVal = e.getLeft().accept(new Eval());
                   |double rightVal = e.getRight().accept(new Eval());
                   |if (leftVal == 0 || rightVal == 0) {
                   |  return new Lit(0.0);
                   |} else if (leftVal == 1) {
                   |  return e.getRight().accept(this);
                   |} else if (rightVal == 1) {
                   |  return e.getLeft().accept(this);
                   |} else {
                   |  return new Mult(e.getLeft().accept(this), e.getRight().accept(this));
                   |}
                   |""".stripMargin).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Divd] { case _ =>
              Java(
                s"""
                   |double leftVal = e.getLeft().accept(new Eval());
                   |double rightVal = e.getRight().accept(new Eval());
                   |if (leftVal == 0) {
                   |  return new Lit(0.0);
                   |} else if (rightVal == 1) {
                   |  return e.getLeft().accept(this);
                   |} else if (leftVal == rightVal) {
                   |  return new Lit(1.0);
                   |} else if (leftVal == -rightVal) {
                   |  return new Lit(-1.0);
                   |} else {
                   |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
                   |}
                   |""".stripMargin).statements()
            }
          )
      },

      oldGenerator
    )
  }


  /**
    * Create test case code for eval where the expression "identifier"  has already been constructed
    * and the test case is UnitTest, which has its own expectations.
    *
    * Forms chain of responsibility
    */
  abstract override def testCaseGenerator(op:Operation, identifier:SimpleName, tc: UnitTest) : Seq[Statement] = {

    if (op.equals(new Collect)) {
      val num: Int = nextTestNumber()
      Java(s"""|  String result$num = (String) ${identifier.toString}.accept(new PrettyP());
               |  assertEquals("${tc.expected.toString}", result$num);
               |""".stripMargin).statements()

      var expected:String = "java.util.List<Double> match = new java.util.ArrayList<Double>();"

      expected += tc.expected.asInstanceOf[java.util.List[expression.instances.Lit]].asScala.map(value => {
        "match.add(" + value.value + ");"
      }).mkString("\n")

      Java(s"""|  java.util.List<Double> result$num = (java.util.List<Double>) ${identifier.toString}.accept(new Collect());
               |  $expected
               |  assertEquals(match, result$num);
               |""".stripMargin).statements()
    } else if (op.equals(new SimplifyExpr)) {
      val num: Int = nextTestNumber()

      val expectedCode: Option[com.github.javaparser.ast.expr.Expression] =
        defaultInstance.instanceGenerators(tc.expected.asInstanceOf[expression.instances.Instance])

      if (expectedCode.isDefined) {
        val initExpected: String = s"""Exp expectedExp${identifier.toString} = ${expectedCode.get.toString};"""

        // TODO: Create an equal visitor for use instead of depending on prettyP
        val str: String =
          s"""
             |  $initExpected
             |  Exp result$num = (Exp) ${identifier.toString}.accept(new SimplifyExpr());
             |  assertEquals(expectedExp${identifier.toString}.accept(new PrettyP()), result$num.accept(new PrettyP()));
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
