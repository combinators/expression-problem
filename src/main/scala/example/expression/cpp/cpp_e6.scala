package example.expression.cpp     /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e6 extends Evolution with CPPGenerator with CPPBinaryMethod with TestGenerator with M0 with M1 with M2 with M3 with M4 with M5 with M6 {
  self:cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 with cpp_e5 =>

  import domain._

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Equals => scala.List[domain.Operation](AsTree)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep): CPPType = {
    tpe match {
      case Boolean => new CPPType("bool")
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[CPPElement] = {

    // generate the actual body; since this is a binary method
    op match {
      case Equals =>
        val opn = domain.AsTree.name
        val expr_e = new CPPElement("e")
        // result(Java(s" $binaryContext$opn().same(that.$opn())").expression[Expression]())
        Seq(new CPPElement(
          s"""
             |Tree *tree1 = (new ${opn.capitalize}(${inBinaryContext(expr_e)}))->getValue();
             |Tree *tree2 = (new ${opn.capitalize}(that))->getValue();
             |${result(new CPPElement("tree1->same(tree2)")).mkString("\n")}
           """.stripMargin))

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testMethod(tests:Seq[domain.TestCase]) : Seq[CPPElement] = {

    // EXTRACT all EqualsBinaryMethodTestCase ones and handle here
    var skip:Seq[domain.TestCase] = Seq.empty

    val stmts:Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1

      test match {
        case eb: EqualsBinaryMethodTestCase =>
          //val code = dependentDispatch(convert(eb.inst1), Equals, convert(eb.inst2))
          val code = binaryDispatch(rec_convert(eb.inst1), Equals, rec_convert(eb.inst2))
          //val tree1 = actual(AsTree, ctc.inst1)
          if (eb.result) {
            Seq(new CPPElement(s"CHECK_TRUE($code);"))
          } else {
            Seq(new CPPElement(s"CHECK_TRUE(!($code));"))
          }
        case _ =>
          skip = skip :+ test
          Seq.empty
      }
    })

    // add these all in to what super produces
    super.testMethod(skip) ++ stmts
  }

  abstract override def testGenerator: Seq[CPPElement] = {
    val tests = new CPPElement(testMethod(M6_tests).mkString("\n"))

    super.testGenerator :+ tests
  }
}
