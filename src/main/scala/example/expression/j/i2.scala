package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{Evolution, I2, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Independent branch to hold Power and Height.
  *
  * By definition, an empty tree has height -1. A tree with a single root node has height 0.
  */
trait i2 extends  Evolution with AbstractGenerator with TestGenerator with I2 {
  self: e0 with e1 with i1 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Integer => Java(s"Integer").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

   abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {

      case Height =>

        val heightPlusOne:Expression = Java(s"${independent.height} + 1").expression[Expression]()
        exp match {
          case _:domain.Binary => Java(
            s"""|return Math.max(${dispatch(subs(domain.base.left), Height, heightPlusOne)},
                |                ${dispatch(subs(domain.base.right), Height, heightPlusOne)});
                |""".stripMargin).statements()

          case _:domain.Unary =>
            Java(s"return ${dispatch(subs(domain.base.inner), Height, heightPlusOne)};").statements()

          case _:domain.Atomic => Java(s"return ${independent.height};").statements()

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val i1 = new domain.UnaryInst(Inv, new LitInst(2.0))
    val a1 = new domain.BinaryInst(Add, new LitInst(5.0), new LitInst(7.0))
    val a2 = new domain.BinaryInst(Add, new LitInst(2.0), new LitInst(3.0))
    val i2 = new domain.UnaryInst(Inv, a2)
    val a3 = new domain.BinaryInst(Add, a1, i2)
    val zero = Java("0").expression[Expression]()

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals(new Integer(1), ${dispatch(convert(i1), Height, zero)});
         |   assertEquals(new Integer(3), ${dispatch(convert(a3), Height, zero)});
         |
         |}""".stripMargin).methodDeclarations()
  }
}
