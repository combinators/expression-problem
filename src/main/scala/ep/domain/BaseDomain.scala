package ep.domain  /*DI:LI:AI*/

/** Foundational trait for all EP domains. */
trait BaseDomain {

  /** Always allow, unless overridden to deny because of reserved word. Not yet Working*/
  var reserved = Array("print", "id")

  // We need to have a consistent strategy for cleaning up
  // these reserved words. Changes based on language. Not Yet Working
  def sanitize(s:String):String = {
    if (reserved.contains(s)) {
      s + "z"
    } else {
      s
    }
  }

  /** There is a base type and subsequent sub-types will extend Types. */
  abstract class TypeRep {
    def name: String = getClass.getName

    /**
      * Request the type as an instance, such as "exp" for the Exp domain.
      *
      * This is useful, for example, for the etiquette of lower case for method names.
      */
    def instance : String = name.toLowerCase

    /**
      * Request the operation as a concept, such as "Exp" for the Exp domain.
      *
      * This is useful, for example, for the etiquette of capitalizing class and interface names.
      */
    def concept : String = name.capitalize
  }
  type BaseTypeRep <: TypeRep
  val baseTypeRep:BaseTypeRep

  // standard attributes for domain. As new ones are defined, create own object to store them
  // Admittedly not the best place
  object base {
    val inner = Attribute("inner", baseTypeRep)
    val left  = Attribute("left", baseTypeRep)
    val right = Attribute("right", baseTypeRep)
    val that  = Parameter("that", baseTypeRep)
  }

  /** Java classes will have attributes and methods reflecting the desired operations. */
  abstract class Element
  case class Attribute(n:String, tpe:TypeRep) extends Element {
    val name:String = sanitize(n)

    /**
      * Request the operation as an instance, such as "eval" for the Eval operation.
      *
      * This is useful, for example, for the etiquette of lower case for method names.
      */
    def instance : String = name.toLowerCase

    /**
      * Request the operation as a concept, such as "Eval" for the Eval operation.
      *
      * This is useful, for example, for the etiquette of capitalizing class and interface names.
      */
    def concept : String = name.capitalize
  }

  /**
    * An operation can have a number of parameters, each of which has a name and a type
    * @param n       name of parameter
    * @param tpe     its type
    */
  case class Parameter(n:String, tpe:TypeRep) extends Element {
    val name: String = sanitize(n)
  }

  /** Each operation is named and has parameters and an optional return type. */
  abstract class Operation(n1:String, val returnType:Option[TypeRep], val parameters:Seq[Parameter] = Seq.empty) extends Element {
    val name:String = sanitize(n1)

    /**
      * Request the operation as an instance, such as "eval" for the Eval operation.
      *
      * This is useful, for example, for the etiquette of lower case for method names.
      */
    def instance : String = name.toLowerCase

    /**
      * Request the operation as a concept, such as "Eval" for the Eval operation.
      *
      * This is useful, for example, for the etiquette of capitalizing class and interface names.
      */
    def concept : String = name.capitalize
  }

  /** Producer and Binary Methods are tagged. */
  class ProducerOperation(override val name:String, override val parameters:Seq[Parameter] = Seq.empty) extends Operation(name, Some(baseTypeRep), parameters)
  class BinaryMethod(override val name:String, override val returnType:Option[TypeRep]) extends Operation(name, returnType, Seq(base.that))

  /** Special operation that declares underlying support for BinaryMethods. */
  case object Tree extends TypeRep
  class BinaryMethodTreeBase(override val name:String, override val returnType:Option[TypeRep]) extends Operation(name, Some(baseTypeRep))
  case object AsTree extends BinaryMethodTreeBase ("astree", Some(Tree))

  // TODO: Maybe ultimately replace need for BinaryMethodTreeBase since AsTree becomes dependent operation. Also AsTree is not entirely approach
  // TODO: in all cases (i.e., think graph structure) but also one can optimize the need for it away if you have Eq (for Haskell) or .equals for Java

  /** Pre-defined unary/binary subtypes that reflects either a unary or binary structure. This is extensible. */
  abstract class Atomic(n1: String, val attributes: Seq[Attribute]) {
    val name:String = sanitize(n1)

    /**
      * Request the data-type as an instance, such as "add" for the Add data type.
      *
      * This is useful, for example, for the etiquette of lower case for methods and attribute names.
      */
    def instance : String = name.toLowerCase

    /**
      * Request the data-type name as a concept, such as "Add" for Add data type.
      *
      * This is useful, for example, for the etiquette of capitalizing class names.
      */
    def concept : String = name.capitalize
  }
  abstract class Unary(override val name:String) extends Atomic(name, Seq(base.inner))
  abstract class Binary(override val name:String) extends Atomic(name, Seq(base.left, base.right))

  /** For testing, one can construct instances over which test cases can be constructed. */
  class AtomicInst(val e:Atomic, val i:Option[Any])
  class UnaryInst(override val e:Atomic, val inner:AtomicInst) extends AtomicInst(e, None)
  class BinaryInst(override val e:Atomic, val left:AtomicInst, val right:AtomicInst) extends AtomicInst(e, None)

  /**
    * A Test case is determined by the expected result of an operation on a given instance.
    * For simple types, such as doubles and strings, we can rely on the default toString method to work properly,
    * but for more complicated tests (such as AsTree and Equals) we need a more powerful mechanism.
    *
    * The expected result, therefore, is allowed to be an in-line expression
    */
  abstract class TestCase

  // When a test case has a definitive expected value, extend this class
  abstract class TestCaseExpectedValue(val expect:(TypeRep, Any)) extends TestCase

  case class EqualsTestCase(inst:AtomicInst, op:Operation, override val expect:(TypeRep,Any), params:(TypeRep,Any)*)
    extends TestCaseExpectedValue(expect)
  case class NotEqualsTestCase(inst:AtomicInst, op:Operation, override val expect:(TypeRep,Any), params:(TypeRep,Any)*)
    extends TestCaseExpectedValue(expect)

  case class EqualsCompositeTestCase(inst:AtomicInst, ops:Seq[Operation], override val expect:(TypeRep,Any), params:(TypeRep,Any)*)
    extends TestCaseExpectedValue(expect)
}
