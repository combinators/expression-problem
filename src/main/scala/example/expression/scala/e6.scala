package example.expression.scala    /*DD:LD:AI*/

import example.expression.domain._
import example.expression.generator.BinaryMethod
import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Determine if structure of two Exps are equal to each other. Checking in.
  *
  * First operation that has parameter which has eExp-recursive structure
  */
trait e6 extends Evolution with ScalaGenerator with TestGenerator with BinaryMethod with M0 with M5 with M6 {
  self: e0 with e1 with e2 with e3 with e4 with e5 =>
  val domain:MathDomain with ModelDomain

  abstract override def typeConverter(tpe:domain.TypeRep): Type = {
    tpe match {
      case Boolean => Type.Name("Boolean")
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {

    // generate the actual body; since this is a binary method
    op match {
      case Equals =>
        val opn = domain.AsTree.name
        val atts= exp.attributes.map(att => Scala(att.name).expression)

        // TODO: very close to replace with. Problems in ExtensibleVisitor (missing methods) as well
        // TODO: as Algebra (since naming conventions don't always work).
        // val that:Expression = Java("that").expression[Expression]()
        // Java(s"return ${delegate(exp,domain.AsTree)}.same(${dispatch(that, domain.AsTree)});").statements
        val that = Scala(s"that").expression
        result(Scala(s"(${delegateFixMe(exp,domain.AsTree,atts:_*)} == ${dependentDispatch(that, domain.AsTree)})").expression)

        // was dispatch(that, domain.AsTree)
        // works for scala_oo
        //Scala(s"""$binaryContext$opn().same(that.$opn())""").statements()

        // OO: astree().same(that.astree())
        // FUNC: new Astree().apply(new Neg(inner)).same(new Astree().apply(that))

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testMethod(tests:Seq[domain.TestCase]) : Stat = {

    // EXTRACT all EqualsBinaryMethodTestCase ones and handle here
    var skip:Seq[domain.TestCase] = Seq.empty

    val stmts:Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1

      test match {
        case eb: EqualsBinaryMethodTestCase =>
          val code = dependentDispatch(convert(eb.inst1), Equals, convert(eb.inst2))

          if (eb.result) {
            Scala(s"assert (true == $code)").statements
          } else {
            Scala(s"assert (false == $code)").statements
          }
        case _ =>
          skip = skip :+ test
          Seq.empty
      }
    })

    // add these all in to what super produces
    addStatements(super.testMethod(skip), stmts)
  }

  abstract override def testGenerator: Seq[Stat] = {
    super.testGenerator :+ testMethod(M6_tests)
  }
}
