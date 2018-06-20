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
  case class Model(name:String, types:Seq[expressions.Exp], ops:Seq[Operation], last:Model) {

    /* Return history of model as a sequence. */
    def toSeq : Seq[Model] = {
      if (isEmpty) {
        Seq(this)
      } else {
        Seq(this) ++ last.toSeq
      }
    }

    /** Return models in evolution order from base (skipping the empty model that is always last). */
    def inOrder:Seq[Model] = toSeq.reverse.tail

    def canEqual(a: Any) : Boolean = a.isInstanceOf[Model]

    /** Suitable equals check for Model. */
    override def equals(that: Any) : Boolean =
      that match {
        case that: Model => that.canEqual(this) && this.hashCode == that.hashCode
        case _ => false
      }

    /** Keep it simple. Hashcode derived solely from name. */
    override def hashCode : Int = {
      name.hashCode
    }

    /** Return flattened model, with same original name. */
    def flat(): Model = {
      toSeq.foldLeft(emptyModel()) {
        case (combined, m) => Model(name, combined.types ++ m.types, combined.ops ++ m.ops, emptyModel())
      }
    }

    /** Determine if operation is supported by this model or any of its antecedents. */
    def supports (op:Operation) : Boolean = {
      if (isEmpty || !ops.contains(op)) {
        false
      } else {
        last.supports(op)
      }
    }

    /** Work backwards to find the most recent Model with an operation. Will return emptyModel if no ops. */
    def lastModelWithOperation() : Model = {
      if (isEmpty || ops.nonEmpty) {
        this
      } else {
        last.lastModelWithOperation()
      }
    }

    /** Work backwards to find the most recent Model with a dataType. Will return emptyModel if no ops. */
    def lastModelWithDataTypes() : Model = {
      if (isEmpty || types.nonEmpty) {
        this
      } else {
        last.lastModelWithDataTypes()
      }
    }

    /** Return the bottommost model in the sequence. */
    def base(): Model = {
      if (last.isEmpty) {
        this
      } else {
        last.base()
      }
    }

    /** A model is empty when it has no dataTypes or operations. */
    def isEmpty: Boolean = types.isEmpty && ops.isEmpty
  }

  /** Useful to be able to construct an empty model. */
  def emptyModel():Model = {
    Model("", Seq.empty, Seq.empty, null)
  }

  // standard attributes for domain. As new ones are defined, place here
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
  class LitInst(d:Double) extends instances.ExpInst(Lit, Some(d))
  val e0:Model = Model("e0", Seq(Lit, Add), Seq(Eval), emptyModel())

  // e1:model evolution
  // -------------------
  case object Sub extends expressions.BinaryExp("Sub")
  val e1:Model = Model (name="e1", Seq(Sub), Seq.empty, e0)

  // e2:model evolution
  // -------------------
  case object String extends types.Types
  case object PrettyP extends Operation("print", Some(String))
  val e2:Model = Model (name="e2", Seq.empty, Seq(PrettyP), e1)

  // e3:model evolution
  // -------------------
  case object Mult extends expressions.BinaryExp("Mult")
  case object Neg extends expressions.UnaryExp("Neg")
  case object Divd extends expressions.BinaryExp("Divd")
  val e3:Model = Model(name="e3", Seq(Neg, Mult, Divd), Seq.empty, e2)

  // e4:model evolution
  // -------------------
  case object Simplify extends Operation("simplify", Some(types.Exp))
  case class List(generic:types.Types) extends types.Types
  case object Collect extends Operation("collect", Some(List(Double)))
  val e4:Model = Model(name="e4",Seq.empty, Seq(Simplify, Collect), e3)

  // e5:model evolution
  case object Boolean extends types.Types
  case object Equal extends Operation("equals", Some(Boolean), ("other", types.Exp))
  val e5:Model = Model(name="e5", Seq.empty, Seq(Equal), e4)

  // e5 straight from e3
  val e5_from_e3:Model = Model(name="e5", Seq.empty, Seq(Equal), e3)

}
