package org.combinators.ep.domain    /*DI:LI:AI*/

/** One can construct instances over which test cases can be constructed and actual code executed. */
/** Can't be case class because other case classes inherit it. */
class Inst(val name:String)

/**
  * Foundational concepts for all EP domains.
  *
  * This type definition is a powerful and elegant way to simply state that the associated instance
  * of this top-level BaseTypeRep is exactly an instance type, [[Inst]]
  *
  * @param baseInst
  */
class BaseDomain(val baseInst:Inst) {

  // This instance removes the need for a Generic Type parameter over (formerly defined) BaseTypeRep
  val baseTypeRep:TypeRep.Aux[Inst] = new TypeRep {
    type scalaInstanceType = Inst

    override def name: String = baseInst.name
  }

  /** Always allow, unless overridden to deny because of reserved word. Not yet Working*/
  var reserved = Array("print", "id")

  // We need to have a consistent strategy for cleaning up
  // these reserved words. Changes based on language. Not Yet Working
  // TODO: nameMangle and this needs to move to language-specific areas.
  def sanitize(s:String):String = {
    if (reserved.contains(s)) {
      s + "z"
    } else {
      s
    }
  }

  // standard attributes for domain. As new ones are defined, create own object to store them
  // Admittedly not the best place
  object base {
    val inner = Attribute("inner", baseTypeRep)
    val left  = Attribute("left", baseTypeRep)
    val right = Attribute("right", baseTypeRep)
    val that  = Parameter("that", baseTypeRep)
  }

  /** Producer and Binary Methods are tagged. */
  class BinaryMethod(override val name:String, override val returnType:TypeRep) extends Operation(name, returnType, Seq(base.that))

  // TODO: Maybe ultimately replace need for BinaryMethodTreeBase since AsTree becomes dependent operation. Also AsTree is not entirely approach
  // TODO: in all cases (i.e., think graph structure) but also one can optimize the need for it away if you have Eq (for Haskell) or .equals for Java
  abstract class Atomic(n1: String, override val attributes: Seq[Attribute]) extends DataType (n1, attributes)
  abstract class Unary(override val name:String) extends DataType(name, Seq(base.inner))
  abstract class Binary(override val name:String) extends DataType(name, Seq(base.left, base.right))

  class AtomicInst(val e:Atomic, val ei:ExistsInstance) extends Inst(e.name)
  class UnaryInst(val e:Unary, val inner:Inst) extends Inst(e.name)
  class BinaryInst(val e:Binary, val left:Inst, val right:Inst) extends Inst(e.name)

  // catch all for any future expansion
  class NaryInst(val e:DataType, val instances:Seq[Inst]) extends Inst(e.name)
  /**
    * A Test case is determined by the expected result of an operation on a given instance.
    * For simple types, such as doubles and strings, we can rely on the default toString method to work properly,
    * but for more complicated tests (such as AsTree and Equals) we need a more powerful mechanism.
    *
    * The expected result, therefore, is allowed to be an in-line expression
    */
  abstract class TestCase

  // When a test case has a definitive expected value, extend this class
  abstract class TestCaseExpectedValue(val expect:ExistsInstance) extends TestCase

  case class EqualsTestCase(inst:Inst, op:Operation, override val expect:ExistsInstance, params:ExistsInstance*)
    extends TestCaseExpectedValue(expect)
  case class NotEqualsTestCase(inst:Inst, op:Operation, override val expect:ExistsInstance, params:ExistsInstance*)
    extends TestCaseExpectedValue(expect)

  case class EqualsCompositeTestCase(inst:Inst, ops:Seq[(Operation, Seq[ExistsInstance])], override val expect:ExistsInstance)
    extends TestCaseExpectedValue(expect)

  case class PerformanceTestCase(
                                  iterations: Int,
                                  bestOf: Int,
                                  op: Operation,
                                  initialInst: Inst,
                                  initialParams: Seq[ExistsInstance],
                                  stepParams: Seq[ExistsInstance] => Seq[ExistsInstance],
                                  stepInstance: Inst => Inst
                                ) extends TestCase


  // Refactored here from the old ModelDomain concept


}
