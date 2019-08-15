package org.combinators.ep.domain    /*DI:LI:AI*/

import abstractions._

/** Models a named domain evolution with new data types and operations, as well as the last evolution.
  *
  * Use `evolve` to obtain the next model and [[org.combinators.ep.domain.Model$.base]] for the initial one.
  *
  * @note Names act as unique identifiers within the context of one domain.
  */
sealed class Model(val name:String, val types:Seq[DataType], val ops:Seq[Operation], val last:Option[Model] = None) {

  /** Adds an evolution to this model.
    *
    * @param name The unique name of this evolution.
    * @param types The new data types.
    * @param ops The new operations.
    */
  def evolve(name:String, types:Seq[DataType], ops:Seq[Operation]): Model =
    new Model(name, types, ops, Some(this))

  /** Returns history of this model as a sequence. */
  def toSeq : Seq[Model] = this +: last.map(_.toSeq).getOrElse(Seq.empty)

  /** Returns models in evolution order from base (skipping the empty model that is always last). */
  def inChronologicalOrder:Seq[Model] = toSeq.reverse.tail

  /** Guard check for equals method. */
  private def canEqual(a: Any) : Boolean = a.isInstanceOf[Model]

  /** Checks two models for equality.
    * Models are uniquely identified by their name.
    */
  override def equals(that: Any) : Boolean =
    that match {
      case that: Model => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  /** Computes the hash code of this model from the hash code of its name. */
  override def hashCode : Int = {
    name.hashCode
  }

  /** Returns a flattened model where all previous evolutions are squashed into a single evolution on top of the
    * base model.
    *
    * The name of the squashed evolution will be the name of this evolution.
    * If no evolutions are present, the model is returned unchanged.
    */
  def flatten: Model = {
    val history = toSeq.reverse
    val (baseModel, evolutions) = (history.head, history.tail)

    def squash(intoModel: Model, nextModel: Model): Model =
      new Model(name, intoModel.types ++ nextModel.types, intoModel.ops ++ nextModel.ops, Some(baseModel))

    if (evolutions.nonEmpty) evolutions.reduceLeft(squash)
    else baseModel
  }

  /** Finds a present or past evolution that defines the given type. */
  def findType(tpe:DataType): Option[Model] =
    toSeq.find(_.types.contains(tpe))

  /** Finds a present or past evolution that defines the given operation. */
  def findOperation(op:Operation): Option[Model] =
    toSeq.find(_.ops.contains(op))

  /** Determines if operation is supported by this model or any of its antecedents. */
  def supports(op:Operation) : Boolean =
    findOperation(op).nonEmpty

  /** Find the most recent Model with an operation. */
  def lastModelWithOperation(): Option[Model] =
    toSeq.find(_.ops.nonEmpty)

  /** Finds the most recent Model with a data type. */
  def lastModelWithDataTypes: Option[Model] =
    toSeq.find(_.types.nonEmpty)

  /** Finds all past data types, starting with the most recent ones. */
  def pastDataTypes: Seq[DataType] =
    toSeq.flatMap(_.types)

  /** Find all past operations, starting with the most recent ones. */
  def pastOperations: Seq[Operation] =
    toSeq.flatMap(_.ops)

  /** Return the bottommost model in the sequence. */
  def base: Model = toSeq.last

  /** Return the datatype defined in the bottom most evolution */
  def baseDataType: DataType = toSeq.last.types.head

  /** A model is empty when it has no dataTypes or operations. */
  def isEmpty: Boolean = types.isEmpty && ops.isEmpty

  /** Construct new linear extension graph consistent with these two models. */
  def merge(name:String, other:Model) : Model = {
    def combineEvolutions(lastEvolution: Model, evolutions: (Option[Model], Option[Model])): Model =
      evolutions match {
        case (Some(e1), Some(e2)) =>
          new Model(
            Seq(e1.name, e2.name).distinct.mkString(":"),
            (e1.types ++ e2.types).distinct,
            (e1.ops ++ e2.ops).distinct,
            Some(lastEvolution)
          )
        case (None, Some(e)) => new Model(e.name, e.types, e.ops, Some(lastEvolution))
        case (Some(e), None) => new Model(e.name, e.types, e.ops, Some(lastEvolution))
        case _ => lastEvolution
      }
    val extendedHistory = toSeq.reverse.map(Some(_)).zipAll(other.toSeq.reverse.map(Some(_)), None, None)
    extendedHistory.reduceLeft(combineEvolutions)
  }

  /**
    * Determines if model contains any binary methods.
    *
    * Typical usage is to call getModel.flatten before calling this method.
    */
  def hasBinaryMethod: Boolean =
    toSeq.contains {
      case _ : BinaryMethod => true
      case _ => false
    }

  /**
    * Determine if model contains any producer operations.
    *
    * Typical usage is to call getModel.flatten before calling this method.
    */
  def hasProducerOperation: Boolean =
    toSeq.contains {
      case _ : BinaryMethod => true
      case _ => false
    }

  /**
    * Determines if this model comes before the given model in the evolution history.
    *
    * Note that if models are the same then return false.
    */
  def before(other:Model): Boolean =
    this != other && other.toSeq.contains(this)
}

object Model {
  /** Provides a base domain model to start evolving from.
    *
    * @param domainName The name of the domain which is being modeled.
    * @param baseTypeName The name of the data type that is being modeled.
    */
  def base(domainName: String, baseTypeName: String): Model =
    new Model(domainName, Seq(DataType(baseTypeName, Seq.empty)), Seq.empty)
}