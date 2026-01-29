package org.combinators.ep.domain.abstractions    /*DI:LI:AI*/

import org.combinators.ep.domain.{GenericModel, abstractions}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.matchers.Matchable

/** Models a named data type. */
case class DataType(name: String)

/**
 * Models a named case of a data type with its attributes.
 *
 * Evolution adds cases to an existing data type.
 */
case class DataTypeCase(name: String, attributes: Seq[Attribute]) {
  /** Returns if this data type case contains any attributes of the data type modeled in the
   * implicitly given domain model.
   */
  def isRecursive(implicit domain: GenericModel): Boolean =
    attributes.exists(_.tpe == TypeRep.DataType(domain.baseDataType))

  /** Returns if this data type case contains no attributes of the data type modeled in the
   * implicitly given domain model.
   */
  def isNonRecursive(implicit domain: GenericModel): Boolean = !isRecursive
}

/** Default ways to construct and destruct data type cases. */
object DataTypeCase {
  /** Constructs a type case without attributes. */
  def atomic(name: String): DataTypeCase = DataTypeCase(name, Seq.empty)

  /** Constructs a type case with a single attribute "inner" of the base data type
   * of the implicitly given domain model.
   */
  def unary(name: String)(implicit domain: GenericModel): DataTypeCase =
    DataTypeCase(name, Seq(Attribute.inner))

  /** Constructs a type case with a two attributes "left" and "right" of the base data type
   * of the implicitly given domain model.
   */
  def binary(name: String)(implicit domain: GenericModel): DataTypeCase =
    DataTypeCase(name, Seq(Attribute.left, Attribute.right))

  /** Returns a [[org.combinators.ep.domain.matchers.Matchable]] transforming the given function on type
   * case attributes to a partial function on attributes of type cases the name of this case.
   *
   * Example:
   * {{{
   *   val fooCase = DataTypeCase("foo", Seq(a1, a2))
   *   val barCase = DataTypeCase("bar", Seq(a3, a4))
   *   Seq(fooCase, barCase).collect(toNamedNaryMatchable(fooCase).matcher(atts => atts.head)) // returns Seq(a1)
   * }}}
   */
  implicit def toNamedNaryMatchable(dtc: DataTypeCase): Matchable[DataTypeCase, Seq[Attribute]] =
    Matchable.named(
      dtc.name,
      Matchable.nary[DataTypeCase, Attribute]({ case x => x.attributes}).matcher
    )

  /** Returns a [[org.combinators.ep.domain.matchers.Matchable]] transforming the given function on a single type
   * case attribute to a partial function on type cases with the name of this case and exactly one attribute.
   *
   * Example:
   * {{{
   *   val foo1Case = DataTypeCase("foo", Seq(a1, a2))
   *   val foo2Case = DataTypeCase("foo", Seq(a3))
   *   Seq(foo1Case, foo2Case).collect(toNamedUnaryMatchable(foo1Case).matcher(att => att)) // returns Seq(a3)
   * }}}
   */
  implicit def toNamedUnaryMatchable(dtc: DataTypeCase): Matchable[DataTypeCase, Attribute] =
    Matchable.named(
      dtc.name,
      Matchable.unary[DataTypeCase, Attribute]({ case x => x.attributes}).matcher
    )

  /** Returns a [[org.combinators.ep.domain.matchers.Matchable]] transforming the given function on two type
   * case attributes to a partial function on type cases with the name of this case and exactly two attributes.
   *
   * Example:
   * {{{
   *   val foo1Case = DataTypeCase("foo", Seq(a1, a2))
   *   val foo2Case = DataTypeCase("foo", Seq(a3))
   *   Seq(foo1Case, foo2Case).collect(toNamedBinaryMatchable(foo1Case).matcher(_.2)) // returns Seq(a2)
   * }}}
   */
  implicit def toNamedBinaryMatchable(dtc: DataTypeCase): Matchable[DataTypeCase, (Attribute, Attribute)] =
    Matchable.named(
      dtc.name,
      Matchable.binary[DataTypeCase, Attribute]({ case x => x.attributes }).matcher
    )
}

/** Models a named and typed attribute of a [[DataTypeCase data type case]] */
case class Attribute(name: String, tpe: TypeRep)

/** Provides some default attributes. */
object Attribute {
  /** An attribute "inner" of the base data type of the implicitly given domain model. */
  def inner(implicit domain: GenericModel):Attribute = Attribute("inner", TypeRep.DataType(domain.baseDataType))
  /** An attribute "left" of the base data type of the implicitly given domain model. */
  def left(implicit domain: GenericModel):Attribute = Attribute("left", TypeRep.DataType(domain.baseDataType))
  /** An attribute "right" of the base data type of the implicitly given domain model. */
  def right(implicit domain: GenericModel):Attribute = Attribute("right", TypeRep.DataType(domain.baseDataType))
}

/** Used to mark special operations (needed for isOp logic) and possible useful in other ways. */
trait Tag { }

/** Models a named operation on a [[DataType]] with parameters and a return type,
 * which defaults to Unit (no return value).
 */
case class Operation(name: String,
                     returnType: TypeRep = TypeRep.Unit,
                     parameters: Seq[Parameter] = Seq.empty, tags:Seq[Tag] = Seq.empty) {

  /** Determines if this is a binary method, which has at least one parameter of
   * the base type of the implicitly given domain model.
   *
   * In original formulation, was restricted to ONLY operations with a single parameter
   */
  def isBinary(implicit domain: GenericModel): Boolean =
    parameters.exists(param => param.tpe == TypeRep.DataType(domain.baseDataType))

  /** Determines if this is a producer operation, which returns an instance of
   * the base type of the implicitly given domain model.
   */
  def isProducer(implicit domain: GenericModel): Boolean =
    returnType == TypeRep.DataType(domain.baseDataType)
}

/** Provides some default operations. */
object Operation {
  /** Models an "astree" operation, which converts a data type instance to a domain representation
   * [[org.combinators.ep.domain.tree.Tree]].
   */
  val asTree: Operation = Operation("astree", TypeRep.Tree)

  /** Returns a [[org.combinators.ep.domain.matchers.Matchable]] transforming the given function on
   * [[org.combinators.ep.domain.abstractions.Parameter Parameters]] and a
   * [[org.combinators.ep.domain.abstractions.TypeRep return type representation]] to a a partial function on a
   * signature of operations of the same name as the given operation.
   *
   * Example:
   * {{{
   *   val fooOp1 = Operation("foo", r1, Seq(p1, p2))
   *   val fooOp2 = Operation("foo", r2, Seq(p3, p4))
   *   val barOp = Operation("bar", r5, Seq(p6, p7))
   *   Seq(fooOp1, fooOp2, barOp).collect(toNamedNaryMatchable(fooOp1).matcher {
   *     case (ps, r) => ps
   *   }) // returns Seq(p1, p2, p3, p4)
   * }}}
   */
  implicit def toNamedNaryMatchable(dtc: DataTypeCase): Matchable[DataTypeCase, Seq[Attribute]] =
    Matchable.named(
      dtc.name,
      Matchable.nary[DataTypeCase, Attribute]({ case x => x.attributes }).matcher
    )
}

/** Models a named and typed parameter of an [[Operation]]. */
case class Parameter(name: String, tpe: TypeRep)

/** Provides some default parameters. */
object Parameter {
  /** A parameter "that" of the base data type of the implicitly given domain model. */
  def that(implicit domain: GenericModel):Parameter = Parameter("that", TypeRep.DataType(domain.baseDataType))
}

/** Represents a host language (Scala) type within the domain.
 *
 * @see [[org.combinators.ep.domain.abstractions.TypeRep]] for helpers to construct representations.
 */
trait TypeRep {
  /** The type to represent. */
  type HostType

  def isModelBase(model:GenericModel):Boolean = this == TypeRep.DataType(model.baseDataType)
}

/** Provides helpers to construct domain representations of host language types. */
object TypeRep {
  /** Representation of the host type `T`. */
  type OfHostType[T] = TypeRep { type HostType = T }

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
  /** Represents the Scala type `Char`. */
  case object Char extends TypeRep {
    type HostType = scala.Char
  }
  /** Represents the Scala type `Double`. */
  case object Double extends TypeRep {
    type HostType = scala.Double
  }
  /** Represents the Scala type `Boolean`. */
  case object Boolean extends TypeRep {
    type HostType = scala.Boolean
  }
  /** Represents the type [[org.combinators.ep.domain.tree.Tree]]. */
  case object Tree extends TypeRep {
    type HostType = org.combinators.ep.domain.tree.Tree
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


  /** Represents a Scala model of an instance of the given domain specific data type. */
  case class DataType(tpe: abstractions.DataType) extends TypeRep {
    type HostType = DataTypeInstance
  }
}

/** Marks any inheriting object as a model of a software test case. */
trait TestCase {
  val tags:Seq[Tag] = Seq.empty
}

/** Models a test case which applies operation `op` to `domainObject` and `params`, expecting a result
 * equal to `expected`. */
case class EqualsTestCase(
                           baseTpe: DataType,
                           domainObject: DataTypeInstance,
                           op: Operation,
                           override val tags:Seq[Tag],
                           expected: InstanceRep,
                           params: InstanceRep*
                         ) extends TestCase

object EqualsTestCase {
  def apply(baseTpe: DataType,
           domainObject: DataTypeInstance,
           op: Operation,
           expected: InstanceRep,
           params: InstanceRep*) : EqualsTestCase = EqualsTestCase(baseTpe, domainObject, op, Seq.empty, expected, params:_*)
}

/** Models a test case which applies operation `op` to `domainObject` and `params`, expecting a result
 * not equal to `expected`. */
case class NotEqualsTestCase(
                              baseTpe: DataType,
                              domainObject: DataTypeInstance,
                              op: Operation,
                              override val tags:Seq[Tag],
                              expected: InstanceRep,
                              params: InstanceRep*
                            ) extends TestCase

object NotEqualsTestCase {
  def apply(baseTpe: DataType,
            domainObject: DataTypeInstance,
            op: Operation,
            expected: InstanceRep,
            params: InstanceRep*) : NotEqualsTestCase = NotEqualsTestCase(baseTpe, domainObject, op, Seq.empty, expected, params:_*)
}

/** Models a test case which sequentially applies the operations `ops` with their given parameters to the
 * `startObject` and then the results of the previous operation, expecting a final result equal to `expected`.
 *
 * Example:
 * {{{
 *   EqualsCompositeTestCase(o1, r, (op1, Seq(p1, p2)), (op2, Seq(p3)))
 *   // After compilation:
 *   op2(op1(o1, p1, p2), p3).equals(r)
 * }}}
 */
case class EqualsCompositeTestCase(
                                    baseTpe: DataType,
                                    override val tags:Seq[Tag],
                                    startObject: DataTypeInstance,
                                    expected: InstanceRep,
                                    ops: (Operation, Seq[InstanceRep])*
                                  ) extends TestCase

object EqualsCompositeTestCase {
  def apply(baseTpe: DataType,
           startObject: DataTypeInstance,
           expected: InstanceRep,
           ops: (Operation, Seq[InstanceRep])*) : EqualsCompositeTestCase = EqualsCompositeTestCase(baseTpe, Seq.empty, startObject, expected, ops:_*)
}

/** Models a performance test case.
 *
 * This test case performs `iterations` many iterations of applying operation `op` and measures the minimal time
 * taken when repeating this process `bestOf` times.
 * After each step, function `stepInstance` is applied to the previous instance to compute the instance for the
 * next step.
 * Parameters for the next step are computed using function `stepInstance`.
 */
case class PerformanceTestCase(
                                iterations: Int,
                                bestOf: Int,
                                op: Operation,
                                baseTpe: DataType,
                                initialObject: DataTypeInstance,
                                initialParams: Seq[InstanceRep],
                                stepParams: Seq[InstanceRep] => Seq[InstanceRep],
                                stepInstance: DataTypeInstance => DataTypeInstance,
                                override val tags:Seq[Tag] = Seq.empty
                              ) extends TestCase