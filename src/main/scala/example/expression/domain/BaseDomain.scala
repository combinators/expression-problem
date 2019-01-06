package example.expression.domain  /*DI:LI:AI*/

/** Foundational trait for all EP domains. */
trait BaseDomain {

  //x type Expression                           /** Base concept for a single expression in language. */
  //x def expression(s:String) : Expression     /** Method to produce expression from arbitrary string. */

  /** Always allow, unless overridden to deny because of reserved word. Not yet Working*/
  var reserved = Array("print")

  // We need to have a consistent strategy for cleaning up
  // these reserved words. Changes based on language. Not Yet Working
  def sanitize(s:String):String = s

  /** There is a base type and subsequent sub-types will extend Types. */
  abstract class TypeRep {
    def name: String = getClass.getName
  }
  type BaseTypeRep <: TypeRep
  val baseTypeRep:BaseTypeRep

  // standard attributes for domain. As new ones are defined, create own object to store them
  // Admittedly not the best place
  object base {
    val inner:String = "inner"
    val left:String = "left"
    val right:String = "right"
    val that:String = "that"
  }


  /** Java classes will have attributes and methods reflecting the desired operations. */
  abstract class Element
  case class Attribute(n1:String, tpe:TypeRep) extends Element {
    val name:String = sanitize(n1)
  }

  /** Each operation is named and has parameters and an optional return type. */
  abstract class Operation(n1:String, val returnType:Option[TypeRep], val parameters:Seq[(String, TypeRep)] = Seq.empty) extends Element {
    val name:String = sanitize(n1)
  }

  /** Producer and Binary Methods are tagged. */
  class ProducerOperation(override val name:String, override val parameters:Seq[(String, TypeRep)] = Seq.empty) extends Operation(name, Some(baseTypeRep), parameters)
  class BinaryMethod(override val name:String, override val returnType:Option[TypeRep]) extends Operation(name, returnType, Seq((base.that, baseTypeRep)))

  /** Special operation that declares underlying support for BinaryMethods. */
  case object Tree extends TypeRep
  class BinaryMethodTreeBase(override val name:String, override val returnType:Option[TypeRep]) extends Operation(name, Some(baseTypeRep))
  case object AsTree extends BinaryMethodTreeBase ("astree", Some(Tree))

  // TODO: Maybe ultimately replace need for BinaryMethodTreeBase since AsTree becomes dependent operation. Also AsTree is not entirely approach
  // TODO: in all cases (i.e., think graph structure) but also one can optimize the need for it away if you have Eq (for Haskell) or .equals for Java

  /** Return unique subtype instance given subtype. A bit like getClass. Take an instance of basetype and return subtype identifier. */
  case class Identifier(override val name:String) extends TypeRep
  class SubTypeIdentifier(override val name:String, override val returnType:Option[Identifier]) extends Operation(name, Some(baseTypeRep))

  /** Pre-defined unary/binary subtypes that reflects either a unary or binary structure. This is extensible. */
  abstract class Atomic(n1: String, val attributes: Seq[Attribute]) {
    val name:String = sanitize(n1)
  }
  abstract class Unary(override val name:String) extends Atomic(name, Seq(Attribute(base.inner, baseTypeRep)))
  abstract class Binary(override val name:String) extends Atomic(name, Seq(Attribute(base.left, baseTypeRep), Attribute(base.right, baseTypeRep)))

  /** For testing, one can construct instances over which test cases can be constructed. */
  class AtomicInst(val e:Atomic, val i:Option[Any])
  class UnaryInst(override val e:Atomic, val inner:AtomicInst) extends AtomicInst(e, None)
  class BinaryInst(override val e:Atomic, val left:AtomicInst, val right:AtomicInst) extends AtomicInst(e, None)

  /**
    * A Test case is determined by the expected result of an operation on a given instance.
    * For simple types, such as doubles and strings, we can rely on the default toString method to work properly,
    * but for more complicated tests (such as AsTree and Equals) we need a more powerful mechanism.
    *
    * The expected result, therefore, is allowed to be an in-line expression
    */
  abstract class TestCase

  // When a test case has a definitive expected value, extend this class
  abstract class TestCaseExpectedValue(val expect:(TypeRep, Any)) extends TestCase

  case class EqualsTestCase(inst:AtomicInst, op:Operation, override val expect:(TypeRep,Any), params:(TypeRep,Any)*)
    extends TestCaseExpectedValue(expect)
  case class NotEqualsTestCase(inst:AtomicInst, op:Operation, override val expect:(TypeRep,Any), params:(TypeRep,Any)*)
    extends TestCaseExpectedValue(expect)
  case class EqualsCompositeTestCase(inst:AtomicInst, ops:Seq[Operation], override val expect:(TypeRep,Any), params:(TypeRep,Any)*)
    extends TestCaseExpectedValue(expect)

//  // when asked, will return an instance of type T
//  abstract class Dispatch[T](val op:Operation) extends TypeRep {
//    def apply() : T
//  }
//
//  // in is typed as Any since it really will be code expressions (in some language)
//  abstract case class RecursiveApply[T](d:Dispatch[T], override val op:Operation) extends Dispatch[T](op)
//  abstract case class BaseApply[T](in:AtomicInst, override val op:Operation) extends Dispatch[T](op)

}
