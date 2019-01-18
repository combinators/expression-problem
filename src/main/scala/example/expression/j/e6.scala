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

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {

    // generate the actual body; since this is a binary method
    op match {
      case Equals =>
        val opn = domain.AsTree.name

        // GOAL: requesting AsTree on self produces same tree as invoking
        // AsTree on that.

        // TODO: very close to replace with. Problems in ExtensibleVisitor (missing methods) as well
        // TODO: as Algebra (since naming conventions don't always work).
        // val that:Expression = Java("that").expression[Expression]()
        // Java(s"return ${delegate(exp,domain.AsTree)}.same(${dispatch(that, domain.AsTree)});").statements
       result(Java(s" $binaryContext$opn().same(that.$opn())").expression[Expression]())

//        val that = Scala(s"that").expression
//        result(Scala(s"(${delegateFixMe(exp,domain.AsTree,atts:_*)} == ${dependentDispatch(that, domain.AsTree)})").expression)


      case _ => super.logic(exp, op)
    }
  }

  abstract override def testMethod(tests:Seq[domain.TestCase]) : MethodDeclaration = {

    // EXTRACT all EqualsBinaryMethodTestCase ones and handle here
    var skip:Seq[domain.TestCase] = Seq.empty

    val stmts:Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1

      test match {
        case eb: EqualsBinaryMethodTestCase =>

          if (eb.result) {
            Java(s"assertTrue (${dispatch(convert(eb.inst1), Equals, convert(eb.inst2))});").statements
          } else {
            Java(s"assertFalse(${dispatch(convert(eb.inst1), Equals, convert(eb.inst2))});").statements
          }
        case _ =>
          skip = skip :+ test
          Seq.empty
      }
    })

    // add these all in to what super produces
    addStatements(super.testMethod(skip), stmts)
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator :+ testMethod(M6_tests)
  }
}
