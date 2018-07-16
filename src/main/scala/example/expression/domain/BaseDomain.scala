package example.expression.domain  /*DI:LI:AI*/

/** Foundational trait for all EP domains. */
trait BaseDomain {

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
  case class Attribute(name:String, tpe:TypeRep) extends Element

  /** Each operation is named and has parameters and an optional return type. */
  abstract class Operation(val name:String, val returnType:Option[TypeRep], val parameters:Seq[(String, TypeRep)] = Seq.empty) extends Element

  /** Producer and Binary Methods are tagged. */
  class ProducerOperation(override val name:String, override val parameters:Seq[(String, TypeRep)] = Seq.empty) extends Operation(name, Some(baseTypeRep), parameters)
  class BinaryMethod(override val name:String, override val returnType:Option[TypeRep]) extends Operation(name, returnType, Seq((base.that, baseTypeRep)))

  /** Pre-defined unary/binary subtypes that reflects either a unary or binary structure. This is extensible. */
  abstract class Atomic(val name: String, val attributes: Seq[Attribute])
  abstract class Unary(override val name:String) extends Atomic(name, Seq(Attribute(base.inner, baseTypeRep)))
  abstract class Binary(override val name:String) extends Atomic(name, Seq(Attribute(base.left, baseTypeRep), Attribute(base.right, baseTypeRep)))

  /** For testing, one can construct instances over which test cases can be constructed. */
  class AtomicInst(val e:Atomic, val i:Option[Any])
  class UnaryInst(override val e:Atomic, val inner:AtomicInst) extends AtomicInst(e, None)
  class BinaryInst(override val e:Atomic, val left:AtomicInst, val right:AtomicInst) extends AtomicInst(e, None)
}
