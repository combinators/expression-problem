package example.expression.domain

/**
  * Mathematical Expressions domain as an instance of EP.
  */
trait MathDomain extends BaseDomain with ModelDomain {
  case object Exp extends TypeRep {
    override def name:String = "Exp"
  }
  type BaseTypeRep = Exp.type
  val baseTypeRep:BaseTypeRep = Exp

  // standard attributes for domain. As new ones are defined, place here
  object attributes {
    val value:String = "value"
  }

  // m0:model evolution.
  // -------------------
  case object Double extends TypeRep
  case object Lit extends Atomic("Lit", Seq(Attribute(attributes.value, Double)))
  case object Add extends Binary("Add")

  case object Eval extends Operation("eval", Some(Double))
  class LitInst(d:Double) extends AtomicInst(Lit, Some(d))
  val m0 = Model("m0", Seq(Lit, Add), Seq(Eval))

  // m1:model evolution
  // -------------------
  case object Sub extends Binary("Sub")
  val m1 = Model("m1", Seq(Sub), Seq.empty, last=m0)

  // m2:model evolution
  // -------------------
  case object String extends TypeRep
  case object PrettyP extends Operation("print", Some(String))
  val m2 = Model("m2", Seq.empty, Seq(PrettyP), last=m1)

  // m3:model evolution
  // -------------------
  case object Mult extends Binary("Mult")
  case object Neg extends Unary("Neg")
  case object Divd extends Binary("Divd")
  val m3 = Model("m3", Seq(Neg, Mult, Divd), Seq.empty, last=m2)

  // m4:model evolution
  // -------------------
  case object Simplify extends ProducerOperation("simplify")
  case class List(generic:TypeRep) extends TypeRep
  case object Collect extends Operation("collect", Some(List(Double)))
  val m4 = Model("m4",Seq.empty, Seq(Simplify, Collect), last=m3)

  // mx: graft in a useful operation for future operations to use
  case object JavaClass extends TypeRep
  case object GetJavaClass extends Operation ("getJavaClass", Some(JavaClass))
  val m4i = Model("mx", Seq.empty, Seq(GetJavaClass), last=m4)

  // m5:model evolution
  case object Boolean extends TypeRep
  case object Equal extends BinaryMethod("equals", Some(Boolean))
  val m5 = Model("m5", Seq.empty, Seq(Equal), last=m4i)
}
