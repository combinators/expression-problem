package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{Evolution, MathDomain, M4i}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait ex extends Evolution with AbstractGenerator with TestGenerator with BinaryMethod with M4i {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case JavaClass => Java(s"java.lang.Class<?>").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)

    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances
      case GetJavaClass => Java(s"""return $getJavaClass;""").statements()

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new domain.BinaryInst(Sub, new LitInst(9.0), new LitInst(112.0))

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertEquals(${dispatch(convert(s2), GetJavaClass)}, ${dispatch(convert(s1), GetJavaClass)});
         |}""".stripMargin).methodDeclarations()
  }
}
