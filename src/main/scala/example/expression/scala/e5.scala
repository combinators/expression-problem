package example.expression.scala   /*DD:LD:AI*/

import example.expression.domain._

import scala.meta.{Stat, Type}

/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with ScalaGenerator with TestGenerator with OperationDependency with M0 with  M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case domain.AsTree => scala.List[domain.Operation](Identifier)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.Tree => Type.Name("tree.Tree")      // package class goes here.
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree => {
        val atts = subExpressions(exp)

        // TODO: replace hard-coded DefinedSubTypes with dependent operation getSubTypeIdentifier and dispatch accordingly.

        // different strategies have different means of accessing attributes, either directly or via
        // getXXX methods. This logic method must defer that knowledge to later.
        // "this" is only valid expression when datatype as class
        exp match {   // was $litValue     ;
          case Lit =>   // ${exp.hashCode()}

            val attParams = atts.map(att => att._2.toString).mkString(",")
            result(Scala(s"""new tree.Node(Seq(new tree.Leaf($attParams)), ${identify(exp, Identifier, atts(litValue))}) """).expression)

          case Neg =>
            val params = atts.map(att => att._2.toString + ".astree()").mkString(",")
            val seq = atts.map(att => dispatch(att._2, domain.AsTree)).mkString(",")
            result(Scala(s"""new tree.Node(Seq($seq), ${identify(exp, Identifier, atts(domain.base.inner))} ) """).expression)

          case Add|Sub|Mult|Divd|Neg =>
            val params = atts.map(att => att._2.toString + ".astree()").mkString(",")
            val seq = atts.map(att => dispatch(att._2, domain.AsTree)).mkString(",")
            result(Scala(s"""new tree.Node(Seq($seq), ${identify(exp, Identifier, atts(domain.base.left), atts(domain.base.right))} ) """).expression)
          }
      }
      case _ => super.logic(exp, op)
    }
  }

  abstract override def testMethod(tests:Seq[domain.TestCase]) : Stat = {
    // EXTRACT all SameTestCase ones and handle here
    var skip: Seq[domain.TestCase] = Seq.empty

    val stmts: Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1
      val idx = pair._2

      val id: String = s"c$idx"

      test match {
        case ctc: SameTestCase =>
          // assertFalse(${dispatch(convert(m5_s1), domain.AsTree)}.same(${dispatch(convert(m5_s2), domain.AsTree)}));
          // we can't just call dispatch because for some (scala_func) there no context in which dispatch can work.
          // in other words, dispatch is local to the dataTypes. SO this can break if dispatch isn't properly global
          val tree1 = dependentDispatch(convert(ctc.inst1), domain.AsTree)
          val tree2 = dependentDispatch(convert(ctc.inst2), domain.AsTree)

          // TODO: Dispatch inappropriate here since test case has different context

          val same = Scala(s"$tree1.same($tree2)").expression

          if (ctc.result) {
            Scala(s"assert(true == $same)").statements
          } else {
            Scala(s"assert(false == $same)").statements
          }
        case _ =>
          skip = skip :+ test
          Seq.empty
      }
    })

    // add these all in to what super produces, which is:
    addStatements(super.testMethod(skip), stmts)
  }

  abstract override def testGenerator: Seq[Stat] = {
    super.testGenerator :+ testMethod(M5_tests)
  }
}
