package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Determine if structure of two Exps are equal to each other. Checking in.
  */
trait e5 extends AbstractGenerator with TestGenerator {
  val domain:Domain

  import domain._

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case Equal =>
        val atts:Map[String,Expression] = subExpressions(exp)
        val name:String = op.parameters.head._1
        val other:types.Types = op.parameters.head._2

        exp match {
          case Lit => {
            val value:Expression = Java(s"((${exp.name.capitalize})$name).${subs(attributes.value)}").expression()

            Java(s"""return ${recurseOnWithParams(subs(attributes.value), Equal, value)};""").statements()
          }

          // left.equals(((Add)other).left) && right.equals(((Add)other).right);
          case ui:expressions.UnaryExp => {
            val inner:Expression = Java(s"((${exp.name.capitalize})$name).${subs(attributes.exp)}").expression()

            Java(s"""return $name.getClass().equals(getClass()) && ${recurseOnWithParams(subs(attributes.exp), Equal, inner)};""").statements()
          }

          case bi:expressions.BinaryExp => {
            val leftExpr:Expression = Java(s"((${exp.name.capitalize})$name).${subs(attributes.left)}").expression()
            val rightExpr:Expression = Java(s"((${exp.name.capitalize})$name).${subs(attributes.right)}").expression()

            Java(s"""return $name.getClass().equals(getClass()) && ${recurseOnWithParams(subs(attributes.left), Equal, leftExpr)} && ${recurseOnWithParams(subs(attributes.right), Equal, rightExpr)};""").statements()
          }

          case _ => super.methodBodyGenerator(exp)(op)
        }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(model:Model): Seq[MethodDeclaration] = {
    val s1 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new BinaryInst(Add, new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0)),
                                 new BinaryInst(Add, new LitInst(5.0), new LitInst(6.0)))
    val s3 = new BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))

      super.testGenerator(model.last) ++ Java(
      s"""
         |public void test() {
         |   assertFalse(${recurseOnWithParams(convert(s1, model), Equal, convert(s2, model))});
         |   assertTrue(${recurseOnWithParams(convert(s1, model), Equal, convert(s3, model))});
         |}""".stripMargin).methodDeclarations()
  }
}
