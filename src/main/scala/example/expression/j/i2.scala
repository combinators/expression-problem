package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.IndependentDomain
import org.combinators.templating.twirl.Java

/**
  * Independent branch to hold Power and Height.
  *
  * By definition, an empty tree has height -1. A tree with a single root node has height 0.
  */
trait i2 extends AbstractGenerator with TestGenerator {
  val domain:IndependentDomain


  abstract override def typeGenerator(tpe:domain.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Integer => Java(s"Integer").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

   abstract override def logic(exp:domain.subtypes.Exp)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {

      case domain.Height =>

        val heightPlusOne:Expression = Java(s"${domain.independent.height} + 1").expression[Expression]()
        exp match {
          case _:domain.subtypes.BinaryExp => Java(
            s"""|return Math.max(${recurseOn(subs(domain.base.left), domain.Height, heightPlusOne)},
                |                ${recurseOn(subs(domain.base.right), domain.Height, heightPlusOne)});
                |""".stripMargin).statements()

          case _:domain.subtypes.UnaryExp =>
            Java(s"return ${recurseOn(subs(domain.base.exp), domain.Height, heightPlusOne)};").statements()

          case _:domain.subtypes.Exp => Java(s"return ${domain.independent.height};").statements()

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val i1 = new domain.UnaryInst(domain.Inv, new domain.LitInst(2.0))
    val a1 = new domain.BinaryInst(domain.Add, new domain.LitInst(5.0), new domain.LitInst(7.0))
    val a2 = new domain.BinaryInst(domain.Add, new domain.LitInst(2.0), new domain.LitInst(3.0))
    val i2 = new domain.UnaryInst(domain.Inv, a2)
    val a3 = new domain.BinaryInst(domain.Add, a1, i2)
    val zero = Java("0").expression[Expression]()

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals(new Integer(1), ${recurseOn(convert(i1), domain.Height, zero)});
         |   assertEquals(new Integer(3), ${recurseOn(convert(a3), domain.Height, zero)});
         |
         |}""".stripMargin).methodDeclarations()
  }
}
