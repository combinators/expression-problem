package org.combinators.ep.domain    /*DI:LI:AI*/

import abstractions._

// Ignore IntelliJ ScalaDoc error: https://youtrack.jetbrains.com/issue/SCL-14638
/** Models a named domain evolution with new data type cases and operations, as well as the last evolution.
  *
  * Use `evolve` to obtain the next model and [[org.combinators.ep.domain.Model$.base]] for the initial one.
  *
  * @note Names act as unique identifiers within the context of one domain.
  */
sealed class Model(
  val name:String,
  val typeCases:Seq[DataTypeCase],
  val ops:Seq[Operation],
  val last:Option[Model] = None) {

  /** Adds an evolution to this model.
    *
    * @param name The unique name of the next evolution.
    * @param types The new data types.
    * @param ops The new operations.
    */
  def evolve(name:String, types:Seq[DataTypeCase], ops:Seq[Operation]): Model =
    new Model(name, types, ops, Some(this))

  /** Returns history of this model as a sequence. */
  lazy val toSeq : Seq[Model] = this +: last.map(_.toSeq).getOrElse(Seq.empty)

  /** Returns models in evolution order from base (skipping the base model that is always last). */
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
      new Model(name, intoModel.typeCases ++ nextModel.typeCases, intoModel.ops ++ nextModel.ops, Some(baseModel))

    if (evolutions.nonEmpty) evolutions.reduceLeft(squash)
    else baseModel
  }

  /** Finds a present or past evolution that defines the given data type case. */
  def findTypeCase(tpe: DataTypeCase): Option[Model] =
    toSeq.find(_.typeCases.contains(tpe))

  /** Finds a present or past evolution that defines the given operation. */
  def findOperation(op:Operation): Option[Model] =
    toSeq.find(_.ops.contains(op))

  def supports(tpe: DataTypeCase): Boolean =
    findTypeCase(tpe).nonEmpty

  /** Determines if operation is supported by this model or any of its antecedents. */
  def supports(op:Operation) : Boolean =
    findOperation(op).nonEmpty

  /** Find the most recent Model with an operation. */
  def lastModelWithOperation: Option[Model] =
    toSeq.find(_.ops.nonEmpty)

  /** Finds the most recent Model with a data type case. */
  def lastModelWithDataTypes: Option[Model] =
    toSeq.find(_.typeCases.nonEmpty)

  /** Finds all past data type cases, starting with the most recent ones. */
  def pastDataTypes: Seq[DataTypeCase] =
    toSeq.flatMap(_.typeCases)

  /** Finds all past operations, starting with the most recent ones. */
  def pastOperations: Seq[Operation] =
    toSeq.flatMap(_.ops)

  /** Returns the bottommost model in the sequence. */
  def base: Model = toSeq.last

  /** Returns the base data type of this model. */
  def baseDataType: DataType = base.baseDataType

  /** Returns if this evolution has no data type cases or operations. */
  def isEmpty: Boolean = typeCases.isEmpty && ops.isEmpty

  /** Constructs new linear extension graph consistent with these two models.
    *
    * Example:
    * [ (base, [Ty], []), (e1, [E1], [op1]), (e2, [E2], [op2, op3]) ]
    * [ (base, [Ty], []), (e1, [E1], [op1]), (e3, [E3], [op4, op5]), (e4, [E4], [op6, op7]) ]
    * are merged to
    * [ (base, [Ty], []), (e1, [E1], [op1]), (e2:e3, [E2, E3], [op2, op3, op4, op5]), (e4, [E4], [op6, op7]) ]
    */
  def merge(name:String, other:Model) : Model = {
    def combineEvolutions(lastEvolution: Option[Model], evolutions: (Option[Model], Option[Model])): Option[Model] =
      evolutions match {
        case (Some(e1), Some(e2)) =>
          Some(
            new Model(
              Seq(e1.name, e2.name).distinct.mkString(":"),
              (e1.typeCases ++ e2.typeCases).distinct,
              (e1.ops ++ e2.ops).distinct,
              lastEvolution
            ))
        case (None, Some(e)) => Some(new Model(e.name, e.typeCases, e.ops, lastEvolution))
        case (Some(e), None) => Some(new Model(e.name, e.typeCases, e.ops, lastEvolution))
        case _ => lastEvolution
      }
    val extendedHistory: Seq[(Option[Model], Option[Model])] =
      toSeq.reverse.map(Some(_)).zipAll(other.toSeq.reverse.map(Some(_)), None, None)
    extendedHistory.foldLeft[Option[Model]](None)(combineEvolutions).get
  }

  /**
    * Determines if this model or its history contain any binary methods.
    *
    * Typical usage is to call getModel.flatten before calling this method.
    */
  def hasBinaryMethod: Boolean =
    toSeq.exists(_.ops.exists(_.isBinary(this)))

  /**
    * Determine if this model or its history contain any producer operations.
    *
    * Typical usage is to call getModel.flatten before calling this method.
    */
  def hasProducerOperation: Boolean =
    toSeq.exists(_.ops.exists(_.isProducer(this)))

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
    new Model(domainName, Seq.empty, Seq.empty) {
      override def baseDataType: DataType = DataType(baseTypeName)
    }
}