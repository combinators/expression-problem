package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{Evolution, M5, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with AbstractGenerator with TestGenerator with M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Tree => Java(s"Tree").tpe()      // internal interface
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree => {
        val atts = subExpressions(exp)
        val params = atts.map(att => att._1 + ".astree()").mkString(",")

        exp match {
          case Lit => Java(s"""return new Node(java.util.Arrays.asList(new Leaf($litValue)), DefinedSubtypes.${exp.name.capitalize}); """).statements
          case Add|Sub|Mult|Divd|Neg => Java(s""" return new Node(java.util.Arrays.asList($params), DefinedSubtypes.${exp.name.capitalize}); """).statements
        }
      }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new domain.BinaryInst(Sub, new LitInst(9.0), new LitInst(112.0))
    val s3 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertFalse(${dispatch(convert(s1), domain.AsTree)}.same(${dispatch(convert(s2), domain.AsTree)}));
         |   assertTrue (${dispatch(convert(s1), domain.AsTree)}.same(${dispatch(convert(s3), domain.AsTree)}));
         |}""".stripMargin).methodDeclarations()
  }
}
