package ep.haskell     /*DD:LD:AI*/

import ep.domain._

/**
  * BinaryMethod capability
  */
trait e5 extends Evolution with HaskellGenerator with HUnitTestGenerator with HaskellBinaryMethod with M0 with M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain with ModelDomain
  import domain._

  abstract override def typeConverter(tpe:TypeRep) : HaskellType = {
    tpe match {
      case Tree => new HaskellType(s"Tree")  // internal interface
      case _ => super.typeConverter(tpe)
    }
  }

  /** If any new imports are needed for an operation, just extend here. */
  override def addedImports(op:domain.Operation):Seq[Haskell] = {
    op match {
      case AsTree => Seq(Haskell("import DataTypes"))
      case _ => super.addedImports(op)
    }
  }

  /** Provide reasonable default values for newly defined types. */
  abstract override def standardDefault(tpe:TypeRep) : Haskell = {
    tpe match {
      case Tree => new Haskell("Leaf 0")    // TODO: might not correct
      case _ => super.standardDefault(tpe)
    }
  }

  abstract override def logic(exp:Atomic, op:domain.Operation): Seq[Haskell] = {
    val source = Source (exp, op)
    // generate the actual body
    op match {
      // Simplify only works for solutions that instantiate expression instances
      case AsTree => {
        val declType = exp.name

        val children = exp match {
          case Lit => Seq(Haskell(s"Leaf ${expression(exp, litValue)}"))
          case _ =>
            exp.attributes.map(att => { contextDispatch(source, deltaChildOp(source, att, AsTree))})
        }
        result(Haskell(s" Node ${declType}Type [ ${children.mkString(",")} ]"))
      }

      case _ => super.logic(exp, op)
    }
  }

  /**
    * Override testMethod to cover [[SameTestCase]] situations.
    *
    * @param test    test case to inspect
    * @param id      current number
    * @return
    */
  override def hunitTestMethod(test:TestCase, id:Int) : Seq[Haskell] = {

    test match {
      case ctc: SameTestCase =>
        val source = NoSource()
        val tree1 = contextDispatch(source, deltaExprOp(source, convert(ctc.inst1), AsTree))
        val tree2 = contextDispatch(source, deltaExprOp(source, convert(ctc.inst2), AsTree))

//        val same = Haskell(s"$tree1.same($tree2)")

        if (ctc.result) {
          Seq(new Haskell(s"""test_v$id = TestCase (assertBool "${test.getClass.getSimpleName}" ($tree1 == $tree2))"""))
        } else {
          Seq(new Haskell(s"""test_v$id = TestCase (assertBool "${test.getClass.getSimpleName}" ($tree1 /= $tree2))"""))
        }
      case _ => super.hunitTestMethod(test, id)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    super.testGenerator :+ hunitMethod(M5_tests)
    //    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    //    val s2 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    //    val s3 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(999.0))

    //    super.testGenerator :+ new Haskell(
    //      s"""
    //         |s1 = ${convert(s1)}
    //         |s2 = ${convert(s2)}
    //         |s3 = ${convert(s3)}
    //         |
    //         |test_e5_1 = TestCase (assertEqual "AsTree-s1" (${AsTree.name}  s1) (${AsTree.name}  s2))
    //         |test_e5_2 = TestCase (assertBool "AsTree-s1" ((${AsTree.name}  s1) /= (${AsTree.name}  s3)))
    //         |
    //         |test_e5 = TestList [ TestLabel "1" test_e5_1, TestLabel "2" test_e5_2 ]
    //         |
    //         |main :: IO Counts
    //         |main  = runTestTT test_e5
    //         |""".stripMargin)
  }

}
