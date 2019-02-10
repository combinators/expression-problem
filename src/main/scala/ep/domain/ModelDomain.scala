package ep.domain  /*DI:LI:AI*/

/**
  * Definition of the Model case class used to represent the Model for any EP solution.
  */
trait ModelDomain extends BaseDomain {

  /** Each model consists of a collection of Exp sub-types and operations. */
  case class Model(name:String, types:Seq[Atomic], ops:Seq[Operation], last:Model = emptyModel()) {

    /* Return history of model as a sequence. */
    def toSeq : Seq[Model] = {
      if (isEmpty) {
        Seq(this)
      } else {
        if (last != null) {
          Seq(this) ++ last.toSeq
        } else {
          Seq(this)
        }
      }
    }

    /** Return models in evolution order from base (skipping the empty model that is always last). */
    def inChronologicalOrder:Seq[Model] = toSeq.reverse.tail

    /** Guard check for equals method. */
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
    def flatten(): Model = {
      toSeq.foldLeft(Model("", Seq.empty, Seq.empty, null)) {
        case (combined, m) => Model(name, combined.types ++ m.types, combined.ops ++ m.ops, Model("", Seq.empty, Seq.empty, null))
      }
    }

    /** Find Model entry in the past that defines type. */
    def findType(tpe:Atomic) : Model = {
      if (isEmpty || types.contains(tpe)) {
        this
      } else {
        last.findType(tpe)
      }
    }

    /** Find Model entry in the past that defines operations. */
    def findOperation(op:Operation) : Model = {
      if (isEmpty || ops.contains(op)) {
        this
      } else {
        last.findOperation(op)
      }
    }

    /** Determine if operation is supported by this model or any of its antecedents. */
    def supports (op:Operation) : Boolean = {
      if (isEmpty) {
        false
      } else if (ops.contains(op)) {
        true
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
    def pastDataTypes(): Seq[Atomic] = {
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

    /** Construct new linear extension graph consistent with these two models. */
    def merge(name:String, other:Model) : Model = {
      var n_me = inChronologicalOrder
      var n_o  = other.inChronologicalOrder
      var head:Model = emptyModel()

      /** If nothing in common, return empty model. */
      if (!n_me.head.equals(n_o.head)) {
        return head
      }

      // find last one that is common
      while (n_o.nonEmpty && n_me.nonEmpty) {
        // still the same
        if (n_o.head.equals(n_me.head)) {
          head = n_o.head
        } else {
          // merge both and record new head
          head = Model(n_o.head.name + ":" + n_me.head.name,
            n_o.head.types ++ n_me.head.types,
            n_o.head.ops ++ n_me.head.ops, head)
        }

        n_o = n_o.tail
        n_me = n_me.tail
      }

      // if we get here, and either one is non empty, just keep extending
      while (n_me.nonEmpty) {
        head = Model(n_me.head.name, n_me.head.types, n_me.head.ops, head)
        n_me = n_me.tail
      }

      while (n_o.nonEmpty) {
        head = Model(n_o.head.name, n_o.head.types, n_o.head.ops, head)
        n_o = n_o.tail
      }

      // Make sure we return topmost Model with proper name
      Model(name, head.types, head.ops, head.last)
    }
  }

  /** Useful to be able to construct an empty model. */
  def emptyModel():Model = {
    Model("", Seq.empty, Seq.empty, null)
  }
}