package org.combinators.cogen

/** Represents a host language (Scala) type within the domain.
  *
  * @see [[org.combinators.ep.domain.abstractions.TypeRep]] for helpers to construct representations.
  */
trait TypeRep {
  /** The type to represent. */
  type HostType
}

/** Provides helpers to construct domain representations of host language types. */
object TypeRep {
  /** Representation of the host type `T`. */
  type OfHostType[T] = TypeRep {type HostType = T}

  /** Represents the Scala type `Unit`. */
  case object Unit extends TypeRep {
    type HostType = scala.Unit
  }

  /** Represents the Scala type `String` */
  case object String extends TypeRep {
    type HostType = java.lang.String
  }

  /** Represents the Scala type `Int`. */
  case object Int extends TypeRep {
    type HostType = scala.Int
  }

  /** Represents the Scala type `Double`. */
  case object Double extends TypeRep {
    type HostType = scala.Double
  }

  /** Represents the Scala type `Boolean`. */
  case object Boolean extends TypeRep {
    type HostType = scala.Boolean
  }

  /** Represents the type `Seq[A]` */
  case class Sequence[T](elemTpe: TypeRep.OfHostType[T]) extends TypeRep {
    type HostType = Seq[T]
  }

  /** Represents the type `Array[T]` */
  case class Array[T](elemTpe: TypeRep.OfHostType[T]) extends TypeRep {
    type HostType = Array[T]
  }

  /** Represents the type A => B */
  case class Arrow[A, B](src: TypeRep.OfHostType[A], tgt: TypeRep.OfHostType[B]) extends TypeRep {
    type HostType = A => B
  }
}