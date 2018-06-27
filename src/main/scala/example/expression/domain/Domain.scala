package example.expression.domain

trait Domain extends BaseDomain with ModelDomain {

  // standard attributes for domain. As new ones are defined, place here
  object attributes {
    val value:String = "value"
  }

  // e0:model evolution.
  // -------------------
  case object Double extends types.Types
  case object Lit extends expressions.Exp("Lit", Seq(Attribute(attributes.value, Double)))
  case object Add extends expressions.BinaryExp("Add")

  case object Eval extends Operation("eval", Some(Double))
  class LitInst(d:Double) extends ExpInst(Lit, Some(d))
  val e0 = Model("e0", Seq(Lit, Add), Seq(Eval), emptyModel())

  // e1:model evolution
  // -------------------
  case object Sub extends expressions.BinaryExp("Sub")
  val e1 = Model(name="e1", Seq(Sub), Seq.empty, e0)

  // e2:model evolution
  // -------------------
  case object String extends types.Types
  case object PrettyP extends Operation("print", Some(String))
  val e2 = Model(name="e2", Seq.empty, Seq(PrettyP), e1)

  // e3:model evolution
  // -------------------
  case object Mult extends expressions.BinaryExp("Mult")
  case object Neg extends expressions.UnaryExp("Neg")
  case object Divd extends expressions.BinaryExp("Divd")
  val e3 = Model(name="e3", Seq(Neg, Mult, Divd), Seq.empty, e2)

  // e4:model evolution
  // -------------------
  case object Simplify extends Operation("simplify", Some(Exp))
  case class List(generic:types.Types) extends types.Types
  case object Collect extends Operation("collect", Some(List(Double)))
  val e4 = Model(name="e4",Seq.empty, Seq(Simplify, Collect), e3)

  // e5:model evolution
  case object Boolean extends types.Types
  case object Equal extends Operation("equals", Some(Boolean), ("other", Exp))
  val e5 = Model(name="e5", Seq.empty, Seq(Equal), e4)
}
