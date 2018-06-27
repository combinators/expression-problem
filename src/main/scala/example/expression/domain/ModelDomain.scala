package example.expression.domain

trait ModelDomain extends BaseDomain {

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
      toSeq.foldLeft(Model("", Seq.empty, Seq.empty, null)) {
        case (combined, m) => Model(name, combined.types ++ m.types, combined.ops ++ m.ops, Model("", Seq.empty, Seq.empty, null))
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

    /** Find past dataTypes. */
    def pastDataTypes(): Seq[expressions.Exp] = {
      if (isEmpty) {
        Seq.empty
      } else {
        types ++ last.pastDataTypes()
      }
    }

    /** Find past operations. */
    def pastOperations(): Seq[Operation] = {
      if (isEmpty) {
        Seq.empty
      } else {
        ops ++ last.pastOperations()
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
}