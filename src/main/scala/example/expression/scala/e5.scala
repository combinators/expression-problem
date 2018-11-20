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

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree => {
        val atts = subExpressions(exp)

        // TODO: replace hard-coded DefinedSubTypes with dependent operation getSubTypeIdentifier and dispatch accordingtly.

        // different strategies have different means of accessing attributes, either directly or via
        // getXXX methods. This logic method must defer that knowledge to later.
        // "this" is only valid expression when datatype as class
        exp match {   // was $litValue     ;
          case Lit =>   // ${exp.hashCode()}

            val attParams = atts.map(att => att._2.toString).mkString(",")
            Scala(s"""new tree.Node(Seq(new tree.Leaf($attParams)), ${delegate(exp, Identifier)}) """).statements()

          case Add|Sub|Mult|Divd|Neg =>
            val params = atts.map(att => att._2.toString + ".astree()").mkString(",")
            Scala(s"""new tree.Node(Seq($params), ${delegate(exp, Identifier)} ) """).statements()
          }
      }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Stat] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new domain.BinaryInst(Sub, new LitInst(9.0), new LitInst(112.0))
    val s3 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))

    super.testGenerator ++ Scala(
      s"""
         |def test() : Unit =  {
         |   assert (false == ${dispatch(convert(s1), domain.AsTree)}.same(${dispatch(convert(s2), domain.AsTree)}));
         |   assert (true == ${dispatch(convert(s1), domain.AsTree)}.same(${dispatch(convert(s3), domain.AsTree)}));
         |}""".stripMargin).statements()
  }
}
