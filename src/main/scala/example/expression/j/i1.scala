package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{Evolution, I1, MathDomain}
import org.combinators.templating.twirl.Java

/**
  * Independent branch to just contain 'Neg'
  */
trait i1 extends Evolution with AbstractGenerator with TestGenerator with I1 {
  self: e0 with e1 =>
  val domain:MathDomain

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    
    // generate the actual body
    op match {

      case Eval => {
        exp match {
          case Inv => Java(s"""return 1 / ${recurseOn(subs(domain.base.inner), Eval)}; """).statements()
          case _ => super.logic(exp)(op)
        }
      }
      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val i1 = new domain.UnaryInst(Inv, new LitInst(2.0))

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals(0.5, ${recurseOn(convert(i1), Eval)});
         |
         |}""".stripMargin).methodDeclarations()
  }
}