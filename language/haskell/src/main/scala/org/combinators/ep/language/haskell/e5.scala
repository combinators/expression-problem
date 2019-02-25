package org.combinators.ep.language.haskell      /*DD:LD:AI*/

import org.combinators.ep.domain.math.{M0, M5, MathDomain}
import org.combinators.ep.domain.{Evolution, ModelDomain}

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
      case Tree => new Haskell("Leaf 0")    // TODO: might not be correct
      case _ => super.standardDefault(tpe)
    }
  }

  abstract override def logic(exp:DataType, op:domain.Operation): Seq[Haskell] = {
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
        val source = NoSource
        val tree1 = contextDispatch(source, deltaExprOp(source, toTargetLanguage(ctc.inst1), AsTree))
        val tree2 = contextDispatch(source, deltaExprOp(source, toTargetLanguage(ctc.inst2), AsTree))

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
  }

}
