package example.expression.domain

/** Foundational trait for all EP domains. */
trait BaseDomain {

  /** There is a base type, Exp, and subsequent sub-types will extend Types. */
  abstract class Types
  case object Exp extends Types

  // standard attributes for domain. As new ones are defined, create own object to store them
  object base {
    val exp:String = "exp"
    val left:String = "left"
    val right:String = "right"
  }

  /** Pre-defined unary/binary subtypes that reflects either a unary Expression or a binary Expression  This is extensible. */
  object subtypes {
    abstract class Exp(val name: String, val attributes: Seq[Attribute])
    abstract class UnaryExp(override val name:String) extends Exp(name, Seq(Attribute(base.exp, Exp)))
    abstract class BinaryExp(override val name:String) extends Exp(name, Seq(Attribute(base.left, Exp), Attribute(base.right, Exp)))
  }

  /** For testing, one can construct instances over which test cases can be constructed. */
  abstract class ExpInst(val e:subtypes.Exp, val i:Option[Any])
  class UnaryInst(override val e:subtypes.Exp, val exp:ExpInst) extends ExpInst(e, None)
  class BinaryInst(override val e:subtypes.Exp, val left:ExpInst, val right:ExpInst) extends ExpInst(e, None)

  /** Java classes will have attributes and methods reflecting the desired operations. */
  abstract class Element
  case class Attribute(name:String, tpe:Types) extends Element

  /** Each operation is named and has parameters and an optional return type. */
  abstract class Operation(val name:String, val returnType:Option[Types], val parameters:(String, Types)*) extends Element
}
