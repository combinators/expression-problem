package example.expression.generator    /*DI:LI:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * This trait contains the fundamental abstractions that define an EP approach,
  * regardless of programming language or application domain.
  *
  * At the heart of EP is code that needs to be written that describes how an operation
  * executes on a given data-type. If all data-types were atomic, then this would not be
  * an interesting problem! Instead, data-types are recursive and have structure, thus it
  * is necessary that the logic be constructed using a clear API.
  *
  * === Common Dispatch Scenarios ===
  * # The most common situation is when the logic for an operation on a data-type is composed from
  *   logic applied to the data-types' sub-expressions. For Example, when an ADD data type
  *   has a LEFT and a RIGHT expression, then the logic for Evaluate (in Java) would be:
  *
  * {{{\${dispatch(atts(left),op)} + \${dispatch(atts(right),op)}}}
  *
  * since the full expression is the addition of a LEFT expression and a RIGHT expression.
  *
  * # For producer methods, there is a need to instantiate data-types based on the specific
  *   domain. For example, in C++, when the Simplify operation tries to simplify a multiplication
  *   data-type but fails, it just produces code that instantiates a new Mult data type
  *   by dispatching Simplify to its LEFT and RIGHT children.
  *
  * {{{\$inst(Mult, dispatch(atts(left), Simplify),dispatch(atts(right), Simplify)))}}}
  *
  *   The API for producer methods is defined in [[Producer]].
  *
  * # Sometimes a new operation must be dispatched to a sub-expression. For example, the
  *   Simplify operation can simplify (1 * 8) with just the expression 8, by evaluating the left
  *   child and seeing that its value is 1.
  *
  * # YET TO BE WRITTEN. Occasionally, a different operation needs to be performed on the current
  *   context. This occurs, for example, in the situation where the Equals operation would like to
  *   call AsTree on the current context.
  *
  * 4. YET TO BE WRITTEN. Finally, a different operation needs to be performed on a different context.
  * For example, a Binary Method representing an operation, such as Equals, takes in a parameter which
  * is itself an Expression (i.e., that). How do we perform this different operation on this
  * different context?
  *
  * @groupname api Core API
  * @groupname deltaHelpers DeltaHelpers
  * @groupname context Context
  * @groupname types Parameterized Types
  * @groupname dependency External Dependencies
  *
  * @groupdesc api Fundamental abstractions needed for any language-based solution to EP
  * @groupprio api 0
  *
  * @groupdesc types Each language must define relevant abstractions that map to these types.
  *            It is acceptable if the same structure is used for multiple types (as an example,
  *            review [[example.expression.cpp.CPPElement]])
  * @groupprio types 10
  *
  * @groupdesc context Each language and approach needs different solutions to assemble the logic
  *           for a given (data-type and operation). The top-level concepts are shown here.
  * @groupprio context 20
  *
  * @groupdesc deltaHelpers When weaving together code expressions representing partial fragments
  *           for a given logic, these helper methods are useful in capturing the desired structures.
  * @groupprio deltaHelpers 30
  */
trait LanguageIndependentGenerator {

  /**
    * Any domain that extends BaseDomain with ModelDomain is suitable.
    * @group dependency
    */
  val domain:BaseDomain with ModelDomain
  import domain._

  /**
    * Base concept for the representation of program unit on disk.
    * @group types
    */
  type CompilationUnit

  /**
    * Base concept for a single expression in language.
    * @group types
    */
  type Expression

  /**
    * Base concept for a meaningful type in the language.
    * @group types
    */
  type Type

  /**
    * Base concept for a meaningful line-of-code in the language.
    * @group types
    */
  type Statement

  /**
    * Retrieve model under consideration.
    * @group dependency
    */
  def getModel:Model

  /**
    * For the processed model, return generated code artifacts for solution.
    * @group api
    */
  def generatedCode() : Seq[CompilationUnit]

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated language-specific type associated with the abstract type.
    *
    * By throwing a runtime exception, we terminate any code generation which fails to account
    * for a given type in the model.
    *
    * Note that the top-most type is domain.baseTypeRep. If no converter is defined for the
    * given type, then this method will ultimately throw a scala.NotImplementedError exception.
    *
    * @param tpe    type from model to be converted into language-specific type.
    * @group api
    */
  @throws[NotImplementedError]("if given type has no resolution.")
  def typeConverter(tpe:domain.TypeRep) : Type = {
    throw new scala.NotImplementedError(s"""Unknown Type "$tpe" """)
  }

  /**
    * Return an expression that refers to the given sub-structure of a data-type by a
    * specific attribute
    *
    * @param exp   desired data-type
    * @param att   desired Attribute
    * @group api
    */
  def expression (exp:Atomic, att:Attribute) : Expression = {
    throw new scala.NotImplementedError(s"""Unknown Attribute "${att.name}" for "${exp.name}. """)
  }

  /**
    * Determines the code expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'. These attributes form the keys into this Map,
    * and the values (Expression objects) represent code fragments for accessing these values.
    *
    * @param exp   data subtype being considered
    * @return      Map with entries for each attribute, and the resulting code expressions
    * @group api
    */
  def subExpressions(exp:Atomic) : Map[String, Expression]

  /**
    * Determines the code expression for a single child of a Exp subtype based on its attributes.
    *
    * By throwing a runtime exception, this method terminates any code generation that uses an invalid
    * attribute name by mistake.
    *
    * @param exp   data subtype being considered
    * @param name  name of desired attribute
    * @return      Map with entries for each attribute, and the resulting code expressions
    * @group api
    */
  @throws[scala.NotImplementedError]("If no attribute with 'name' exists.")
  def subExpression(exp:Atomic, name:String) : Expression = {
    throw new scala.NotImplementedError(s"""Unknown Attribute "$name" for "${exp.name}. """)
  }

  /**
    * For all possible EP solutions, this method generates the sequence of statements that result
    * for a given Operation and data-type.
    *
    * Must be Seq since some operations require more substantial implementation depending upon the
    * programming language.
    *
    * Must be Statements (rather than just an Expression) because in most operations, a value of
    * some sort is returned, thus instead of just "expr" it becomes "return expr;" To activate the
    * "return expr;" statement,
    *
    * @param exp    data-type for the context
    * @param op     operation for the context
    * @group api
    */
  @throws[scala.NotImplementedError]("If no (data-type, operation) combination defined.")
  def logic(exp:Atomic, op:Operation) : Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }

  /**
    * Logic produces the sequence of statements that encodes the logic of an operation on a data
    * type. As part of those statements, there is often a computed value that represents the result
    * of the operation. In some languages, this result needs to be returned (i.e., Java or C++); in
    * some EP approaches, this result is simply stored. In other languages (i.e., Scala or functional)
    * there is no specific return statement.
    *
    * In any event, each approach determines how to process the result. It could be handled in
    * language-specific manner and then overridden as needed by the EP approach.
    *
    * @param expr   Expression that represents the result of a log(exp, op).
    * @group api
    */
  def result (expr:Expression) : Seq[Statement]

  /**
    * Responsible for dispatching sub-expressions with possible parameter(s).
    *
    * In an object-oriented-style of programming, there must be an 'expression' on which to base the dispatch.
    * Functional-oriented languages could ignore first field.
    *
    * Intent of this function is to model the execute of operation on children of a datatype.
    *
    * @param expr     expression representing the code fragment into which operation is being performed
    * @param op       desired operation
    * @param params   potential parameters of this operation
    * @group api
    */
  def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression

  /**
    * When defining the logic(exp,op) that represents the code statements for applying a given operation
    * on a specific data-type, it can be defined using sub-expressions that represent the dispatch
    * of that operation (or a new operation) on a different context (or the same context).
    *
    * When dispatching the same operation on a sub-structure of self, then use dispatch; for
    * example, in logic(Mult,Eval), one would state:
    *
    * {{{dispatch(subs(left), Eval) * dispatch(subs(right), Eval)}}}
    *
    * In producer operations such as logic(Mult,Simplify), which tries to simplify the
    * Multiplication operation, there is a need to evaluate the sub-expressions, which means
    * one needs to dispatch Eval to a child sub-expression
    *
    * In binary operations such as logic(Mult,Equals), which tries to determine if two
    * expressions are equal to each other, there is a need to evaluate subexpressions
    * on a different object (i.e., "that").
    *
    * Another option is to delegate a new operation to the current context (rather than just
    * to one of the sub-expressions).
    *
    * All of these options are handled here.
    *
    * All logic is defined within a specific (exp,op) source context, and the
    * Dispatch context represents "Subject Verb Objects+".
    *
    * 1. Exists within source context
    * 2.
    * @group api
    */
  def contextDispatch(source:Context, delta:Delta) : Expression

  /**
    * A Context represents the point in the expression problem where the logic for a given
    * data-type and operation is to be found. It can be considered as "Subject Verb Object*"
    * with a number of objects as parameters.
    *
    * @param exp      (optional) data-type of the context
    * @param op       (optional) operation
    * @param params   (optional) parameters to the operation
    * @group context
    */
  abstract class Context(val exp:Option[Atomic], val op:Option[Operation], val params:Expression*)

  /**
    * When code is being generated independently within a test case, use the @link{TestSource} source context.
    * @group context
    */
  case class TestSource() extends Context(None, None)

  /**
    * Point in expression problem solution where logic is to be inserted for a given data-type, e,
    * and operation, o, with given optional parameters, p.
    *
    * @param e    data-type of interest
    * @param o    operation being performed on the data-type
    * @param p    optional parameters for this operation.
    * @group context
    */
  case class Source(e:Atomic, o:Operation, p:Expression*) extends Context(Some(e), Some(o), p : _*)

  /**
    * The logic for a given source context will typically need to weave together code fragments
    * from different contexts, referred to as [[Delta]].
    *
    * @param expr     code fragment representing expression derived from current context.
    * @param op       (optional) operation
    * @param params   (optional) parameters to the operation
    * @group context
    */
  class Delta(val expr:Option[Expression], override val op:Option[Operation], override val params:Expression*) extends Context(None, op, params : _*) {

    /** Specifies whether this delta exists independently of a prior context. */
    def isIndependent: Boolean = false
  }

  /**
    * When the new context is truly independent from the source context, [[DeltaIndependent]] lets
    * you record the context of the new expression and operation.
    *
    * @param expr     code fragment representing expression derived from current context.
    * @param op       (optional) operation
    * @param params   (optional) parameters to the operation
    * @group context
    */
  class DeltaIndependent(override val expr:Option[Expression], override val op:Option[Operation], override val params:Expression*) extends Delta(expr, op, params : _*) {

    /** [[DeltaIndependent]] exists independently of a prior context. */
    override def isIndependent: Boolean = true
  }

  /**
    * Helper method for creating a [[Delta]] context that represents a new operation (with
    * potential parameters) being applied to a child of the current source context (identified
    * by its attribute name.
    *
    * This method uses [[subExpression]] on the source to generate the code fragment representing
    * the expression to use within the returned [[Delta]]
    *
    * @param source    Source context
    * @param attName   child element, identified by attribute name
    * @param op        operation to perform on the child element
    * @param params    potential paremeters for operation.
    * @group deltaHelpers
    */
  def deltaChildOp(source:Source, attName:String, op:Operation, params:Expression*) : Delta = {
    new Delta(Some(subExpression(source.e, attName)), Some(op), params : _*)
  }

  /**
    * Helper method for creating a [[Delta]] context that represents a new operation (with
    * potential parameters) being applied to a new expression context, passed in as 'expr'
    * parameter.
    *
    * @param source    Source context
    * @param expr      code fragment that will provide part of the new context
    * @param op        designated operation on this new context
    * @param params    potential parameters for this operation
    * @group deltaHelpers
    */
  def deltaExprOp(source:Context, expr:Expression, op:Operation, params:Expression*) : Delta = {
    new Delta(Some(expr), Some(op), params : _*)
  }

  /**
    * Helper method for constructor Delta when requesting operation on a different expression which
    * is truly independent from any context. For now, only used by C++ to avoid issues with getters
    * and setters. Ultimately could be made into a new top-level API like independentDispatch
    *
    * @param source    Source Context
    * @param expr      new expression representing the new context
    * @param op        new operation as part of new context
    * @param params    potential parameters for new operation
    * @group deltaHelpers
    */
  def deltaIndependentExprOp(source:Context, expr:Expression, op:Operation, params:Expression*) : Delta = {
    new DeltaIndependent(Some(expr), Some(op), params : _*)
  }

  /**
    * Helper method for constructor Delta when requesting operation on self.
    *
    * Since the context of source remains the same, the [[Delta]] returned only
    * records the new operation,op, with its parameters.
    *
    * @param source    source Context
    * @param op        desired operation
    * @param params    potential parameters of this operation.
    * @group deltaHelpers
    */
  def deltaSelfOp(source:Source, op:Operation, params:Expression*) : Delta = {
    new Delta(None, Some(op), params : _*)
  }

}
