package example.expression.Pure

trait Pure {

  object types {
    abstract class Types
    case object Exp extends Types
  }

  abstract class Method()
  case class Attribute(name:String, tpe:types.Types) extends Method

  abstract class Operation(val name:String, val returnType:Option[types.Types], val parameters:(String, types.Types)*) extends Method

  object expressions {
    abstract class Exp(val attributes:Attribute*)
    abstract class UnaryExp extends Exp(Attribute("exp", types.Exp))
    abstract class BinaryExp extends Exp(Attribute("left", types.Exp), Attribute("right", types.Exp))
  }

  // Each model consists of a collection of Exp sub-types and operations
  case class Model(name:String, types:Seq[expressions.Exp], ops:Seq[Operation])

  // e0:model evolution.
  case object Double extends types.Types
  val VALUE:String = "value"
  case object Lit extends expressions.Exp(Attribute(VALUE, Double))
  case object Add extends expressions.BinaryExp
  val EVAL:String = "eval"
  case object Eval extends Operation(EVAL, Some(Double))
  val e0:Model = Model("e0", Seq(Lit, Add), Seq(Eval))

  // e1:model evolution
  case object Sub extends expressions.BinaryExp
  val e1:Model = e0.copy(name="e1", types=e0.types :+ Sub)

  // e2:model evolution
  case object String extends types.Types
  val PRINT:String = "print"
  case object PrettyP extends Operation(PRINT, Some(String))
  val e2:Model = e1.copy(name="e2", ops=e1.ops :+ PrettyP)

  // e3:model evolution
  case object Mult extends expressions.BinaryExp
  case object Neg extends expressions.UnaryExp
  case object Divd extends expressions.BinaryExp
  val e3:Model = e2.copy(name="e3", types=e2.types ++ Seq(Neg, Mult, Divd))

}
