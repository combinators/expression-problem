package example.expression.generator    /*DI:LI:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Fundamental trait that contains the fundamental abstractions one needs to define
  * when completing an EP approach, regardless of programming language or application domain.
  *
  * At the heart of EP is code that needs to be written that describes how an operation
  * executes on a given data-type. If all data-types were atomic, then this would not be
  * an interesting problem! Instead, data-types are recursive and have structure, thus it
  * is necessary that the logic be constructed using a clear API.
  *
  * 1. The most common situation is when the logic for an operation on a data-type is composed from
  * logic applied to the data-types' sub-expressions. For Example, when an ADD data type
  * has a LEFT and a RIGHT expression, then the logic for Evaluate (in Java) would be:
  *
  * {{{\${dispatch(atts(base.left),op)} + \${dispatch(atts(base.right),op)}}}
  *
  * since the full expression is the addition of a LEFT expression and a RIGHT expression.
  *
  * 2. For producer methods, there is a need to instantiate data-types based on the specific
  * domain. For example, in C++, when Simplify of the Multiply data-type fails to find a
  * suitable simplification, then it just produces code that instantiates a new Mult data type
  * by dispatching Simplify to its LEFT and RIGHT children.
  *
  * {{{\$inst(Mult, dispatch(atts(base.left), Simplify),dispatch(atts(base.right), Simplify)))
}}}
  *
  * The API is defined in [[example.expression.generator.Producer]].
  *
  * 3. YET TO BE WRITTEN. Occasionally, a different operation needs to be performed on the current
  * context. This occurs, for example, in the situation where the Equals operation would like to
  * call AsTree on the current context.
  *
  * 4. YET TO BE WRITTEN. Finally, a different operation needs to be performed on a different context.
  * For example, a Binary Method representing an operation, such as Equals, takes in a parameter which
  * is itself an Expression (i.e., that). How do we perform this different operation on this
  * different context?
  *
  */
trait LanguageIndependentGenerator {
  /** Any domain that extends BaseDomain with ModelDomain is suitable. */
  val domain:BaseDomain with ModelDomain
  import domain._

  /** Base concept for the representation of program unit on disk. */
  type CompilationUnit

  /** Base concept for a single expression in language. */
  type Expression

  /** Base concept for a meaningful type in the language. */
  type Type

  /** Base concept for a meaningful line-of-code in the language. */
  type Statement

  /** For context dispatch. */
  abstract class Context(val exp:Option[Atomic], val op:Option[Operation], val params:Expression*)
  case class Source(e:Atomic, o:Operation, p:Expression*) extends Context(Some(e), Some(o), p : _*)
  case class TestSource() extends Context(None, None)

  class Delta(val expr:Option[Expression], override val op:Option[Operation], override val params:Expression*) extends Context(None, op, params : _*) {
    def isIndependent: Boolean = false
  }

  class DeltaIndependent(override val expr:Option[Expression], override val op:Option[Operation], override val params:Expression*) extends Delta(expr, op, params : _*) {
    override def isIndependent: Boolean = true
  }

  /** Helper method for constructor Delta when changing both the expression AND operation. */
  def deltaChildOp(source:Source, attName:String, op:Operation, params:Expression*) : Delta = {
    new Delta(Some(subExpression(source.e, attName)), Some(op), params : _*)
  }

  /** Helper method for constructor Delta when requesting operation on a different expression. */
  def deltaExprOp(source:Context, expr:Expression, op:Operation, params:Expression*) : Delta = {
    new Delta(Some(expr), Some(op), params : _*)
  }

  /**
    * Helper method for constructor Delta when requesting operation on a different expression which
    * is truly independent from any context. For now, only used by C++ to avoid issues with getters
    * and setters. Ultimately could be made into a new top-level API like independentDispatch
    */
  def deltaIndependentExprOp(source:Context, expr:Expression, op:Operation, params:Expression*) : Delta = {
    new DeltaIndependent(Some(expr), Some(op), params : _*)
  }

  /** Helper method for constructor Delta when requesting operation on self. */
  def deltaOp(source:Source, op:Operation, params:Expression*) : Delta = {
    new Delta(None, Some(op), params : _*)
  }

  /** Retrieve model under consideration. */
  def getModel:Model

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode() : Seq[CompilationUnit]

  /**
    * Determines the code expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'. These attributes form the keys into this Map,
    * and the values (Expression objects) represent code fragments for accessing these values.
    *
    * @param exp   data subtype being considered
    * @return      Map with entries for each attribute, and the resulting code expressions
    */
  def subExpressions(exp:Atomic) : Map[String, Expression]

  /**
    * Determines the code expression for a single child of a Exp subtype based on its attributes.
    *
    * By throwing a runtime exception, we terminate any code generation that uses an invalid
    * attribute name.
    *
    * @param exp   data subtype being considered
    * @param name  name of desired attribute
    * @return      Map with entries for each attribute, and the resulting code expressions
    */
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
    */
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
    * @param expr   Expression that represents the result of a log(exp, op).
    */
  def result (expr:Expression) : Seq[Statement]

  /**
    * Responsible for dispatching sub-expressions with possible parameter(s).
    *
    * In an object-oriented-style of programming, there must be an 'expression' on which to base the dispatch.
    * Functional-oriented languages could ignore first field.
    *
    * Intent of this function is to model the execute of operation on children of a datatype.
    */
  def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression

  /**
    * When defining the logic(exp,op) that represents the code statements for applying a given operation
    * on a specific data-type, it can be defined using sub-expressions that represent the dispatch
    * of that operation (or a new operation) on a different context (or the same context).
    *
    * Regular dispatch occurs, for example, in logic(Mult,Eval), as in:
    *    dispatch(subs(left), Eval) * dispatch(subs(right), Eval)
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
    * All logic is defined within a specific (exp,op) source context, and the dispatch can
    * Dispatch context represents "Subject Verb Objects+".
    *
    * 1. Exists within source context
    * 2.
    */
  def contextDispatch(source:Context, delta:Delta) : Expression

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated language-specific type associated with the abstract type.
    *
    * By throwing a runtime exception, we terminate any code generation which fails to account
    * for a given type in the model.
    *
    * Note that the top-most type is domain.baseTypeRep.
    */
  def typeConverter(tpe:domain.TypeRep) : Type = {
    throw new scala.NotImplementedError(s"""Unknown Type "$tpe" """)
  }
}
