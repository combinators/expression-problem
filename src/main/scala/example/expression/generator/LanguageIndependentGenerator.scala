package example.expression.generator    /*DI:LI:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

trait LanguageIndependentGenerator {
  val domain:BaseDomain with ModelDomain

  type CompilationUnit     /** Base concept for the representation of program unit on disk. */
  type Expression          /** Base concept for a single expression in language. */
  type Type                /** Base concept for a meaningful type in the language. */
  type Statement           /** Base concept for a meaningful line-of-code in the language. */

  /** Retrieve model under consideration. */
  def getModel:domain.Model

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[CompilationUnit]

  /**
    * Determines the Java expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, Expression]

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
    * Used to have exp:domain.Atomic as 1st parameter.
    */
  def delegateFixMe(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression

  /**
    * Modify compilation unit as needed for extra language-specific mechanisms for allowing generated
    * language to be compiled/interpreted.
    *
    * TBA: Better to have dispatch return Expression *and* obligations for context such as #include or imports
    * TODO: Kick down the road
    *
    * a) hard-code fully-qualified class names
    * b) collect together all compilation units
    * c) other options...
    */
  def addDispatchContext(op:domain.Operation, unit:CompilationUnit) : CompilationUnit = {
    unit
  }

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated Java type associated with the abstract type, with option of a covariant replacement
    * , covariantReplacement:Option[Type] = None
    */
  def typeConverter(tpe:domain.TypeRep) : Type = {
    throw new scala.NotImplementedError(s"""Unknown Type "$tpe" """)
  }

  /**
    * Universal situation across all possible solutions is the sequence of statements that result
    * for a given Operation and data-type.
    */
  def logic(exp:domain.Atomic)(op:domain.Operation) : Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }

}
