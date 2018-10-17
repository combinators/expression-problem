package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain._
import example.expression.generator.BinaryMethod
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Determine if structure of two Exps are equal to each other. Checking in.
  *
  * First operation that has parameter which has eExp-recursive structure
  */
trait e6 extends Evolution with JavaGenerator with TestGenerator with BinaryMethod with M0 with M5 with M6 {
  self: e0 with e1 with e2 with e3 with e4 with e5 =>
  val domain:MathDomain with ModelDomain

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None): com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {

    // generate the actual body; since this is a binary method
    op match {
      case Equals =>
        val opn = domain.AsTree.name


        // TODO: very close to replace with. Problems in ExtensibleVisitor (missing methods) as well
        // TODO: as Algebra (since naming conventions don't always work).
        // val that:Expression = Java("that").expression[Expression]()
        // Java(s"return ${delegate(exp,domain.AsTree)}.same(${dispatch(that, domain.AsTree)});").statements
       Java(s"""return $binaryContext$opn().same(that.$opn());""").statements

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
         |   assertFalse(${dispatch(convert(s1), Equals, convert(s2))});
         |   assertTrue(${dispatch(convert(s1), Equals, convert(s3))});
         |}""".stripMargin).methodDeclarations
  }
}
