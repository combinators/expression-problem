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
trait e6 extends Evolution with JavaGenerator with JUnitTestGenerator with BinaryMethod with M0 with M5 with M6 {
  self: e0 with e1 with e2 with e3 with e4 with e5 =>
  val domain:MathDomain with ModelDomain

  abstract override def typeConverter(tpe:domain.TypeRep): com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe)
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

  abstract override def testMethod(tests:Seq[domain.TestCase]) : MethodDeclaration = {

    // EXTRACT all EqualsBinaryMethodTestCase ones and handle here
    var pass:Seq[domain.TestCase] = Seq.empty
    val local:Seq[domain.TestCase] = tests.filter(p => p match {
      case _:EqualsBinaryMethodTestCase => true
      case _ => false
    })

    val stmts:Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1
      val idx = pair._2

      val id:String = s"c$idx"

      test match {
        case eb: EqualsBinaryMethodTestCase =>

          if (eb.result) {
            Java(s"assertTrue(${dispatch(convert(eb.inst1), Equals, convert(eb.inst2))});").statements
          } else {
            Java(s"assertFalse(${dispatch(convert(eb.inst1), Equals, convert(eb.inst2))});").statements
          }
        case _ =>
          pass = pass :+ test
          Seq.empty
      }
    })

    // add these all in to what super produces
    addStatements(super.testMethod(pass), stmts)
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator :+ testMethod(M6_tests)
  }
}
