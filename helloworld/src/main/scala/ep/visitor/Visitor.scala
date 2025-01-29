/**
 * Taken from  http://infoscience.epfl.ch/record/52625 with upgrades for Scala "2.12.17"
 */
package ep.visitor

trait Base {
  trait Exp { def accept(v: Visitor): Unit }
  class Num(value: Int) extends Exp {
    def accept(v: Visitor): Unit = v.visitNum(value)
  }

  type visitor <: Visitor
  trait Visitor { def visitNum(value: Int): Unit  }

  class Eval extends Visitor {
    var result: Int = _

    def apply(t: Exp): Int = {
      t.accept(this)
      result
    }

    def visitNum(value: Int): Unit = { result = value }
  }
}

object BaseTest extends Base with App {
  type visitor = Visitor
  val op: Eval = new Eval

  Console.println(op.apply(new Num(8)))
}

trait BasePlus extends Base {
  type visitor <: Visitor
  trait Visitor extends super.Visitor {
    def visitPlus(left: Exp, right: Exp): Unit
  }
  class Plus(left: Exp, right: Exp) extends Exp {
    def accept(v: Visitor): Unit =
      v.visitPlus(left, right)
  }
  class Eval extends super.Eval with Visitor {
    def visitPlus(l: Exp, r: Exp): Unit = {
      result = apply(l) + apply(r)
    }
  }
}
//
//trait BaseNeg extends Base {
//  type visitor <: Visitor;
//  trait Visitor extends super.Visitor {
//    def visitNeg(term: Exp): Unit
//  }
//  class Neg(term: Exp) extends Exp {
//    override def accept(v: visitor): Unit =     // TYPO in paper
//      v.visitNeg(term)
//  }
//  class Eval extends super.Eval with Visitor {
//    def visitNeg(term: Exp): Unit = {
//      result = -apply(term)
//    }
//  }
//}
//
//trait BasePlusNeg extends BasePlus with BaseNeg {
//  type visitor <: Visitor
//  trait Visitor extends super.Visitor with super[BaseNeg].Visitor
//  class Eval extends super.Eval with super[BaseNeg].Eval with Visitor
//}
//
//object BasePlusNegTest extends BasePlusNeg {
//  type visitor = Visitor
//  val op: visitor = new Eval
//  Console.println(op.apply(
//    new Plus(new Num(1), new Neg(new Num(2)))));
//}