package org.combinators.cogen

/** A host language (Scala) instance, which can be represented inside of a domain.
  *
  * In type theory this is encoded as a dependent sum, Sigma, where there exists a type and an instance for that type.
  * Check [[https://partialflow.wordpress.com/2017/07/26/dependent-types-type-level-programming/ this introduction]]
  * on dependent types in Scala.
  *
  * The type of the instance is captured by 'tpe'
  *
  * @see The companion model [[org.combinators.cogen.InstanceRep]] for creating new instances.
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
        that.isInstanceOf[InstanceRep] && inst == that.inst
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
}