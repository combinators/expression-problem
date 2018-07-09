package example.expression.j

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{Evolution, M2, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e2 extends Evolution with AbstractGenerator with TestGenerator with M2 {
  self:e0 with e1 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case String => Java("String").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      case PrettyP =>
        exp match {
          case Lit => Java(s"""return "" + ${subs(domain.attributes.value)} + ""; """).statements()
          case Add => Java(s"""return "(" + ${recurseOn(subs(domain.base.left), PrettyP)} + "+" + ${recurseOn(subs(domain.base.right), PrettyP)}+ ")";""").statements()
          case Sub => Java(s"""return "(" + ${recurseOn(subs(domain.base.left), PrettyP)} + "-" + ${recurseOn(subs(domain.base.right), PrettyP)} + ")";""").statements()
          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new domain.BinaryInst(Add, new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0)),
                                 new domain.BinaryInst(Add, new LitInst(5.0), new LitInst(6.0)))

      super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals("(1.0-2.0)", ${recurseOn(convert(s1), PrettyP)});
         |   assertEquals("((1.0-2.0)+(5.0+6.0))", ${recurseOn(convert(s2), PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
