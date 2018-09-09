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

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case Tree => new Haskell("Leaf 0")    // TODO: might not correct
      case _ => super.standardDefault(tpe)
    }
  }

  abstract override def logic(exp:Atomic)(op:domain.Operation): Seq[Haskell] = {
    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances
      case AsTree => {
        val atts = subExpressions(exp)

        val declType = exp.name

        val children:Haskell = exp match {
          case Lit => Haskell(s"Leaf $litValue")
          case _ => Haskell(exp.attributes.map(att => s"(${AsTree.name.toLowerCase} ${att.name})").mkString(","))
        }

        Seq(Haskell(s" Node ${declType}Type [ $children ]"))
      }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s2 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val s3 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(999.0))

    super.testGenerator :+ new Haskell(
      s"""
         |s1 = ${convert(s1)}
         |s2 = ${convert(s2)}
         |s3 = ${convert(s3)}
         |
         |test_e5_1 = TestCase (assertEqual "AsTree-s1" (${AsTree.name}  s1) (${AsTree.name}  s2))
         |test_e5_2 = TestCase (assertBool "AsTree-s1" ((${AsTree.name}  s1) /= (${AsTree.name}  s3)))
         |
         |test_e5 = TestList [ TestLabel "1" test_e5_1, TestLabel "2" test_e5_2 ]
         |
         |main :: IO Counts
         |main  = runTestTT test_e5
         |""".stripMargin)
  }

//    super.testGenerator :+ new Haskell(
//      s"""
//         |public void test() {
//         |   assertEquals(${dispatch(convert(s2), domain.AsTree)}, ${dispatch(convert(s1), domain.AsTree)});
//         |}""".stripMargin)
//  }
}
