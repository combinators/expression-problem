package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Determine if structure of two Exps are equal to each other. Checking in.
  *
  * First operation that has parameter which has Exp-recursive structure
  */
trait e5 extends Evolution with AbstractGenerator with TestGenerator with BinaryMethod with M0 with M4i with M5 {
  self: e0 with e1 with e2 with e3 with e4 with ex =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None): com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case Equal =>
        val atts:Map[String,Expression] = subExpressions(exp)
        val name:String = op.parameters.head._1
        val other:domain.TypeRep = op.parameters.head._2
        val oname = Java(name).expression[Expression]()

        exp match {
          case Lit => {
            val call:String = atts(litValue).toString
            val value:Expression = Java(s"((${exp.name.capitalize})$name).$call").expression()
            Java(s"""return $getJavaClass.equals(${dispatch(oname, GetJavaClass)}) && $call.equals($value);""").statements()
          }

          case ui:domain.Unary => {
            val call:String = atts(domain.base.inner).toString//.substring(2)
            val inner:Expression = Java(s"((${exp.name.capitalize})$name).$call").expression()

            Java(s"""return $getJavaClass.equals(${dispatch(oname, GetJavaClass)}) && ${dispatch(subs(domain.base.inner), Equal, inner)};""").statements()
          }

          case bi:domain.Binary => {
            val leftCall:String = atts(domain.base.left).toString//.substring(2)
            val rightCall:String = atts(domain.base.right).toString//.substring(2)
            val leftExpr:Expression = Java(s"((${exp.name.capitalize})$name).$leftCall").expression()
            val rightExpr:Expression = Java(s"((${exp.name.capitalize})$name).$rightCall").expression()

            Java(s"""return $getJavaClass.equals(${dispatch(oname, GetJavaClass)}) && ${dispatch(subs(domain.base.left), Equal, leftExpr)} && ${dispatch(subs(domain.base.right), Equal, rightExpr)};""").statements()
          }

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new domain.BinaryInst(Add, new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0)),
                                 new domain.BinaryInst(Add, new LitInst(5.0), new LitInst(6.0)))
    val s3 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))

      super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertFalse(${dispatch(convert(s1), Equal, convert(s2))});
         |   assertTrue(${dispatch(convert(s1), Equal, convert(s3))});
         |}""".stripMargin).methodDeclarations()
  }
}
