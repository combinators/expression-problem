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
  val BASE:String = "Exp"

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

  // e4:model evolution
  val SIMPLIFY:String = "simplify"
  case object Simpify extends Operation(SIMPLIFY, Some(types.Exp))
  val COLLECT:String = "collect"
  case class List(generic:types.Types) extends types.Types
  case object Collect extends Operation(COLLECT, Some(List(Double)))
  val e4:Model = e3.copy(name="e4", ops=e3.ops ++ Seq(Simpify, Collect))

 // use following to generate instances to work with

 object instances {
    abstract class ExpInst(val e:expressions.Exp, val i:Option[Any])
    abstract class UnaryExpInst(override val e:expressions.Exp, val exp:ExpInst) extends ExpInst(e, None)
   abstract class BinaryExpInst(override val e:expressions.Exp, val left:ExpInst, val right:ExpInst) extends ExpInst(e, None)
  }

  // construct some sample expressions for testing...
  class LitInst(d:Double) extends instances.ExpInst(Lit, Some(d))
  class BinaryInst(b:expressions.BinaryExp, e1:instances.ExpInst, e2:instances.ExpInst)
    extends instances.BinaryExpInst(b, e1, e2)
  class UnaryInst(b:expressions.UnaryExp, e1:instances.ExpInst)
    extends instances.UnaryExpInst(b, e1)

}
