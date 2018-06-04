package example.expression.domain

trait Domain {

  /** There is a base type, Exp, and subsequent sub-types will extend Types. */
  object types {
    abstract class Types
    case object Exp extends Types
  }

  /** Java classes will have attributes and methods reflecting the desired operations. */
  abstract class Element
  case class Attribute(name:String, tpe:types.Types) extends Element

  /** Each operation is named and has parameters and an optional return type. */
  abstract class Operation(val name:String, val returnType:Option[types.Types], val parameters:(String, types.Types)*) extends Element

  /** Each sub-type reflects either a unary Expression or a binary Expression  This is extensible. */
  object expressions {
    abstract class Exp(val name:String, val attributes:Attribute*)
    abstract class UnaryExp(override val name:String) extends Exp(name, Attribute(attributes.exp, types.Exp))
    abstract class BinaryExp(override val name:String) extends Exp(name, Attribute(attributes.left, types.Exp), Attribute(attributes.right, types.Exp))
  }

  /** For testing, one can construct instances over which test cases can be constructed. */
  object instances {
    abstract class ExpInst(val e:expressions.Exp, val i:Option[Any])
    abstract class UnaryExpInst(override val e:expressions.Exp, val exp:ExpInst) extends ExpInst(e, None)
    abstract class BinaryExpInst(override val e:expressions.Exp, val left:ExpInst, val right:ExpInst) extends ExpInst(e, None)
  }

  /** Canonical instances for a unary or binary instance. */
  class BinaryInst(b:expressions.BinaryExp, e1:instances.ExpInst, e2:instances.ExpInst)
    extends instances.BinaryExpInst(b, e1, e2)
  class UnaryInst(b:expressions.UnaryExp, e1:instances.ExpInst)
    extends instances.UnaryExpInst(b, e1)

  /** Each model consists of a collection of Exp sub-types and operations. */
  case class Model(name:String, types:Seq[expressions.Exp], ops:Seq[Operation])

  // standard attributes
  object attributes {
    val value:String = "value"
    val exp:String = "exp"
    val left:String = "left"
    val right:String = "right"
  }

  // e0:model evolution.
  // -------------------
  case object Double extends types.Types
  case object Lit extends expressions.Exp("Lit", Attribute(attributes.value, Double))
  case object Add extends expressions.BinaryExp("Add")

  case object Eval extends Operation("eval", Some(Double))
  val e0:Model = Model("e0", Seq(Lit, Add), Seq(Eval))
  class LitInst(d:Double) extends instances.ExpInst(Lit, Some(d))

  // e1:model evolution
  // -------------------
  case object Sub extends expressions.BinaryExp("Sub")
  val e1:Model = e0.copy(name="e1", types=e0.types :+ Sub)

  // e2:model evolution
  // -------------------
  case object String extends types.Types
  case object PrettyP extends Operation("print", Some(String))
  val e2:Model = e1.copy(name="e2", ops=e1.ops :+ PrettyP)

  // e3:model evolution
  // -------------------
  case object Mult extends expressions.BinaryExp("Mult")
  case object Neg extends expressions.UnaryExp("Neg")
  case object Divd extends expressions.BinaryExp("Divd")
  val e3:Model = e2.copy(name="e3", types=e2.types ++ Seq(Neg, Mult, Divd))

  // e4:model evolution
  // -------------------
  case object Simplify extends Operation("simplify", Some(types.Exp))
  case class List(generic:types.Types) extends types.Types
  case object Collect extends Operation("collect", Some(List(Double)))
  val e4:Model = e3.copy(name="e4", ops=e3.ops ++ Seq(Simplify, Collect))
}
