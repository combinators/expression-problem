package org.combinators.ep.domain

/** Bundles core abstractions for domain-independent modeling of abstract data types. */
package object abstractions {

  /** Models a named data type with its attributes. */
  case class DataType(name: String, attributes: Seq[Attribute])

  /** Default ways to construct datatypes. */
  object DataType {
    /** Constructs a type without attributes. */
    def atomic(name: String) = DataType(name, Seq.empty)
    /** Constructs a type with a single attribute "inner" of the base data type of the implicitly given domain model. */
    def unary(name: String)(implicit domain: Model) = DataType(name, Seq(Attribute.inner))
    /** Constructs a type with a two attributes "left" and "right" of the base data type of the implicitly given
      *  domain model.
      */
    def binary(name: String)(implicit domain: Model) =
      DataType(name, Seq(Attribute.left, Attribute.right))
  }

  /** Models a named and typed attribute of a [[org.combinators.ep.domain.abstractions.DataType data type]] */
  case class Attribute(name: String, tpe: TypeRep)

  /** Provides some default attributes. */
  object Attribute {
    /** An attribute "inner" of the base data type of the implicitly given domain model. */
    def inner(implicit domain: Model) = Attribute("inner", TypeRep.BaseType)
    /** An attribute "left" of the base data type of the implicitly given domain model. */
    def left(implicit domain: Model)  = Attribute("left", TypeRep.BaseType)
    /** An attribute "right" of the base data type of the implicitly given domain model. */
    def right(implicit domain: Model) = Attribute("right", TypeRep.BaseType)
  }

  /** Models a named operation on a [[DataType]] with parameters and a return type,
    * which defaults to Unit (no return value).
    */
  class Operation(val name: String,
    val returnType: TypeRep = TypeRep.Unit,
    val parameters: Seq[Parameter] = Seq.empty)

  /** Models a named and typed parameter of an [[Operation]].*/
  case class Parameter(name: String, tpe:TypeRep)

  /** Provides some default parameters. */
  object Parameter {
    /** A parameter "that" of the base data type of the implicitly given domain model. */
    def that(implicit domain: Model)  = Parameter("that", TypeRep.BaseType)
  }


  // Ignore IntelliJ error about not finding the companion object in Scala Doc.
  /** Represents a host language (Scala) type within the domain.
    * @see [[org.combinators.ep.domain.abstractions.TypeRep$]] for helpers to construct representations.
    */
  trait TypeRep {
    /** The type to represent. */
    type HostType
  }

  /** Provides helpers to construct domain representations of host language types. */
  object TypeRep {
    /** Representation of the host type `T`. */
    type OfHostType[T] = TypeRep { type HostType = T }

    /** Represents the Scala type `Unit` */
    case object Unit extends TypeRep {
      type HostType = scala.Unit
    }
    /** Represents the Scala type `String` */
    case object String extends TypeRep {
      type HostType = java.lang.String
    }
    /** Represents the Scala type `Int` */
    case object Int extends TypeRep {
      type HostType = scala.Int
    }
    /** Represents the Scala type `Double` */
    object Double extends TypeRep {
      type HostType = scala.Double
    }
    /** Represents the type [[org.combinators.ep.domain.tree.Tree]] */
    object Tree extends TypeRep {
      type HostType = org.combinators.ep.domain.tree.Tree
    }

    /** Represents a given domain specific data type */
    case class DomainSpecific(domainType: DataType) extends TypeRep {
      type HostType = domainType.type
    }

    /** Represents the base type of the implicitly given domain model. */
    def BaseType(implicit domain: Model): DomainSpecific =
      DomainSpecific(domain.baseDataType)
  }

  /** Models binary methods, which have a single parameter of the base type of the implicitly given domain model. */
  class BinaryMethod(name: String, returnType: TypeRep)(implicit domain: Model)
    extends Operation(name, returnType, Seq(Parameter.that))

  /** Models binary methods, which return an instance of the base type of the implicitly given domain model. */
  class ProducerOperation(name:String, parameters: Seq[Parameter] = Seq.empty)(implicit domain: Model)
    extends Operation(name, TypeRep.BaseType, parameters)

  /** Models an "astree" operation, which converts a data type instance to a domain representation
    * [[org.combinators.ep.domain.tree.Tree]].
    */
  class AsTree extends Operation("astree", TypeRep.Tree)

  /** Producer and Binary Methods are tagged. */
  class BinaryMethod(override val name:String, override val returnType:TypeRep) extends Operation(name, returnType, Seq(base.that))




  class AtomicInst(val e:Atomic, val ei:ExistsInstance) extends Inst(e.name)
  class UnaryInst(val e:Unary, val inner:Inst) extends Inst(e.name)
  class BinaryInst(val e:Binary, val left:Inst, val right:Inst) extends Inst(e.name)

  // catch all for any future expansion
  class NaryInst(val e:DataType, val instances:Seq[Inst]) extends Inst(e.name)
  /**
    * A Test case is determined by the expected result of an operation on a given instance.
    * For simple types, such as doubles and strings, we can rely on the default toString method to work properly,
    * but for more complicated tests (such as AsTree and Equals) we need a more powerful mechanism.
    *
    * The expected result, therefore, is allowed to be an in-line expression
    */
  abstract class TestCase

  // When a test case has a definitive expected value, extend this class
  abstract class TestCaseExpectedValue(val expect:ExistsInstance) extends TestCase

  case class EqualsTestCase(inst:Inst, op:Operation, override val expect:ExistsInstance, params:ExistsInstance*)
    extends TestCaseExpectedValue(expect)
  case class NotEqualsTestCase(inst:Inst, op:Operation, override val expect:ExistsInstance, params:ExistsInstance*)
    extends TestCaseExpectedValue(expect)

  case class EqualsCompositeTestCase(inst:Inst, ops:Seq[(Operation, Seq[ExistsInstance])], override val expect:ExistsInstance)
    extends TestCaseExpectedValue(expect)

  case class PerformanceTestCase(
    iterations: Int,
    bestOf: Int,
    op: Operation,
    initialInst: Inst,
    initialParams: Seq[ExistsInstance],
    stepParams: Seq[ExistsInstance] => Seq[ExistsInstance],
    stepInstance: Inst => Inst
  ) extends TestCase
}