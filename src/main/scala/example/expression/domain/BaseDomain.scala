package example.expression.domain  /*DI:LI:AI*/

/** Foundational trait for all EP domains. */
trait BaseDomain {

  /** Always allow, unless overridden to deny because of reserved word. */
  var reserved = Array("print")

  // We need to have a consistent strategy for cleaning up
  // these reserved words. Changes based on language
  def sanitize(s:String):String = s

  /** There is a base type and subsequent sub-types will extend Types. */
  abstract class TypeRep {
    def name: String = getClass.getName
  }
  type BaseTypeRep <: TypeRep
  val baseTypeRep:BaseTypeRep

  // standard attributes for domain. As new ones are defined, create own object to store them
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
}
