package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.IndependentDomain
import org.combinators.templating.twirl.Java

/**
  * Independent branch to just contain 'Neg'
  */
trait i1 extends AbstractGenerator with TestGenerator {
  val domain:IndependentDomain

  abstract override def logic(exp:domain.subtypes.Exp)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    
    // generate the actual body
    op match {

      case domain.Eval => {
        exp match {
          case domain.Inv => Java(s"""return 1 / ${recurseOn(subs(domain.base.exp), domain.Eval)}; """).statements()
          case _ => super.logic(exp)(op)
        }
      }
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val i1 = new domain.UnaryInst(domain.Inv, new domain.LitInst(2.0))

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals(0.5, ${recurseOn(convert(i1), domain.Eval)});
         |
         |}""".stripMargin).methodDeclarations()
  }
}
