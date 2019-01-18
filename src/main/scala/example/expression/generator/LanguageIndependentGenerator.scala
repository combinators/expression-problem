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
  * {{{\$inst(Mult, dispatch(atts(domain.base.left), Simplify),dispatch(atts(domain.base.right), Simplify))).mkString("\n")
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
    * Responsible for identifying the expression representing the given data subtype.
    *
    * In an object-oriented-style of programming, this could be an instantiated object representing
    * the sub-type. For a functional-oriented languages this could be the function name.
    */
  def identify(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression

  /**
    * Responsible for delegating to a new operation on the current data-type context, which
    * is passed in as first parameter.
    *
    * In some cases, possible to ignore the current context, but that is decision to be made
    * by the implementation
    *
    * Should have expr:Expression as context as minimum, rather than just top-level entry
    */
  def delegateFixMe(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression

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
