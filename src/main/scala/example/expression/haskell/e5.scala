package example.expression.haskell     /*DD:LD:AI*/

import example.expression.domain.{Evolution, M5, MathDomain, ModelDomain}
/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with AbstractGenerator with TestGenerator with HaskellBinaryMethod with M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain with ModelDomain
  import domain._

  abstract override def typeConverter(tpe:TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case Tree => new HaskellType(s"Tree")  // internal interface
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:Atomic)(op:domain.Operation): Seq[Haskell] = {
    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances
      case AsTree => {
        val atts = subExpressions(exp)
        val params = standardArgs(exp)
        val invokes = exp.attributes.map(att => s"(${AsTree.name.toLowerCase} ${att.name})").mkString(" ")

        exp match {
          case Lit => Seq(Haskell(s" Leaf ${exp.name}Type $litValue"))
          case Neg => Seq(Haskell(s" Node ${exp.name}Type (${exp.name} ${base.inner}) (${AsTree.name.toLowerCase} ${base.inner}) Nil"))
          case Add|Sub|Mult|Divd => {
            Seq(Haskell(s""" Node ${exp.name}Type (${exp.name} $params) $invokes """))
          }
        }
      }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new domain.BinaryInst(Sub, new LitInst(9.0), new LitInst(112.0))

    super.testGenerator :+ new Haskell(
      s"""
         |public void test() {
         |   assertEquals(${dispatch(convert(s2), domain.AsTree)}, ${dispatch(convert(s1), domain.AsTree)});
         |}""".stripMargin)
  }
}
