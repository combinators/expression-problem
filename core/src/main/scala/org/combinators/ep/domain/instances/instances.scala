package org.combinators.ep.domain.instances

/*DI:LI:AI*/

/** Provides abstractions used to represent type instances of the host language (Scala) in a domain specific context and
  * vice versa domain specific data types in the host language.
  */

import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.{DataTypeCase, TypeRep}

/** A host language (Scala) instance, which can be represented inside of a domain.
  *
  * In type theory this is encoded as a dependent sum, Sigma, where there exists a type and an instance for that type.
  * Check [[https://partialflow.wordpress.com/2017/07/26/dependent-types-type-level-programming/ this introduction]]
  * on dependent types in Scala.
  *
  * The type of the instance is captured by 'tpe'
  *
  * @see The companion model [[org.combinators.ep.domain.instances.InstanceRep]] for creating new instances.
  */
sealed trait InstanceRep {
  /** Provides the domain representation of the instance type. */
  val tpe: TypeRep
  /** Provides the host language (Scala) instance. */
  val inst: tpe.HostType

  override def hashCode(): Int = inst.hashCode()
  override def equals(obj: Any): Boolean = {
    obj match {
      case that: InstanceRep =>
        that.isInstanceOf[InstanceRep.OfHostType[tpe.HostType]] && inst == that.inst
      case _ => false
    }
  }
}

/** Provides methods for wrapping host language (Scala) instances into domain representable entities. */
object InstanceRep {
  /** Type of represented instances of a given host language (Scala) type. */
  type OfHostType[T] = InstanceRep {val tpe: TypeRep.OfHostType[T]}

  /** Creates a new instance representation for the given representable host language type and an instance of it. */
  def apply(tpe: TypeRep)(inst: tpe.HostType): InstanceRep = {
    val tpeArg: tpe.type = tpe
    val instArg: tpeArg.HostType = inst
    case class IR(override val tpe: tpeArg.type)(
      override val inst: tpeArg.HostType
    ) extends InstanceRep
    IR(tpeArg)(instArg)
  }

  /** Creates a new instance representation for the given domain specific data type instance using the
    * base data type of the implicitly given domain model.
    */
  def apply(inst: DataTypeInstance)(implicit domain: GenericModel): InstanceRep = {
    InstanceRep(TypeRep.DataType(domain.baseDataType))(inst)
  }
}

/** Models instances of a domain specific data type in Scala.
  *
  * Types of the `attributeInstances` need to match the attribute types of the data type, which is also checked by
  * a runtime assertion.
  *
  * @param tpeCase            The domain type case to model.
  * @param attributeInstances Instances for all attributes of the domain type.
  */
case class DataTypeInstance(tpeCase: DataTypeCase, attributeInstances: Seq[InstanceRep]) {
  require({
    tpeCase.attributes.corresponds(attributeInstances) { (attribute, attributeInstance) =>
      attribute.tpe == attributeInstance.tpe
    }
  },
    "Attribute instances need to match the attribute types described in the type case"
  )
}
