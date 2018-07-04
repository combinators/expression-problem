package example.expression.domain

/**
  * Mathematical Expressions domain as an instance of EP.
  */
trait Domain extends BaseDomain with ModelDomain {

  // standard attributes for domain. As new ones are defined, place here
  object attributes {
    val value:String = "value"
  }

  // e0:model evolution.
  // -------------------
  case object Double extends Types
  case object Lit extends subtypes.Exp("Lit", Seq(Attribute(attributes.value, Double)))
  case object Add extends subtypes.BinaryExp("Add")

  case object Eval extends Operation("eval", Some(Double))
  class LitInst(d:Double) extends ExpInst(Lit, Some(d))
  val e0 = Model("e0", Seq(Lit, Add), Seq(Eval))

  // e1:model evolution
  // -------------------
  case object Sub extends subtypes.BinaryExp("Sub")
  val e1 = Model("e1", Seq(Sub), Seq.empty, last=e0)

  // e2:model evolution
  // -------------------
  case object String extends Types
  case object PrettyP extends Operation("print", Some(String))
  val e2 = Model("e2", Seq.empty, Seq(PrettyP), last=e1)

  // e3:model evolution
  // -------------------
  case object Mult extends subtypes.BinaryExp("Mult")
  case object Neg extends subtypes.UnaryExp("Neg")
  case object Divd extends subtypes.BinaryExp("Divd")
  val e3 = Model("e3", Seq(Neg, Mult, Divd), Seq.empty, last=e2)

  // e4:model evolution
  // -------------------
  case object Simplify extends Operation("simplify", Some(Exp))
  case class List(generic:Types) extends Types
  case object Collect extends Operation("collect", Some(List(Double)))
  val e4 = Model("e4",Seq.empty, Seq(Simplify, Collect), last=e3)

  // ex: graft in a useful operation for future operations to use
  case object JavaClass extends Types
  case object GetJavaClass extends Operation ("getJavaClass", Some(JavaClass))
  val e4i = Model("ex", Seq.empty, Seq(GetJavaClass), last=e4)

  // e5:model evolution
  case object Boolean extends Types
  case object Equal extends Operation("equals", Some(Boolean), ("other", Exp))
  val e5 = Model("e5", Seq.empty, Seq(Equal), last=e4i)
}
