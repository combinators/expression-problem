package org.combinators.ep.generator    /*DI:LI:AI*/

import org.combinators.ep.domain._
import abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}

// TODO: Cleanup documentation -- Jan
/**
  * This trait contains the fundamental abstractions that define an EP approach,
  * regardless of programming language or application domain.
  *
  * At the heart of EP is code that needs to be written that describes how an operation
  * executes on a given data-type. If all data-types were atomic, then this would not be
  * an interesting problem! Instead, data-types are recursive and have structure, thus it
  * is necessary that the logic be constructed using a clear API. In addition, logic of
  * an operation can be decomposed into different operations on the data-type under consideration.
  *
  * === Common Dispatch Scenarios ===
  *
  *  1. The most common situation is when the logic for an operation on a data-type is composed from
  *     logic applied to the data-types' sub-expressions. For example, when an Add data-type
  *     has a Left and a Right expression, then the logic for Evaluate (in Java) would be:
  *
  *         {{{dispatch(atts(left),op) + dispatch(atts(right),op)}}}
  *
  *     since the full expression is the addition of a Left expression and a Right expression.
  *
  *  2. Sometimes a new operation must be dispatched to a sub-expression. For example, the
  *     Simplify operation can simplify the mathematical expression (9 - 9) with just the
  *     expression 0, by evaluating the value of the left and right children to see if
  *     they are the same.
  *
  *  {{{
  * val deltaLeft = dispatchChild(source.e, left, Eval)
  * val deltaRight = dispatchChild(source.e, right, Eval)
  * Java(s"""|if (\${contextDispatch(source, deltaLeft)} == \${contextDispatch(source, deltaRight)}) {
  * |   \${result(inst(Lit, zero)).mkString("\n")}
  * |} else {
  * |   \${result(inst(Sub, dispatch(expression(exp, left), Simplify),
  * |                      dispatch(expression(exp, right), Simplify))).mkString("\n")}
  * |}""".stripMargin).statements()
  *  }}}
  *
  *  Calling '''contextDispatch''' using deltaChildOp enables one to invoke a different operation (in
  *  this case, Eval is different from the original operation, Simplify) on a child context,
  *  which could be either of the left or right attributes.
  *
  *  3. Occasionally, a different operation needs to be performed on the current context. This
  *     occurs, for example, in the situation where the Equals operation would like to
  *     call AsTree on the current context. Once both tree objects are constructed, it is
  *     straightforward to check for equality by comparing whether these two tree objects
  *     are the same.
  **
  *{{{
  * val deltaLeft = deltaSelfOp(domain.AsTree)
  * val that = Java(domain.base.that.name).expression[Expression]()
  * val deltaRight = deltaExprOp(that, domain.AsTree)
  * val lhs = contextDispatch(source, deltaLeft)
  * val rhs = contextDispatch(source, deltaRight)
  * result(Java(s"\$lhs.same(\$rhs)").expression())
  * }}}
  *
  * Calling '''contextDispatch''' using deltaSelfOp enables one to invoke a different operation
  * (in this case, AsTree is different from Equals) on its self context.
  *
  *  4. Finally, a different operation may need to be performed on a different context.  The
  *     code snippet above shows this example. Calling '''contextDispatch''' using deltaExprOp
  *     enables one to invoke a different operation (in this case, AsTree is different
  *     from Equals) on a different context; in this case, the expression "that" which represents
  *     the argument to the Equals operation.
  *
  * === Producer Methods ===
  *
  * For producer methods, there is a need to instantiate data-types based on the specific
  * domain. For example, in C++, when the Simplify operation tries to simplify a multiplication
  * data-type but fails, it just produces code that instantiates a new Mult data-type
  * by dispatching Simplify to its LEFT and RIGHT children.
  *
  * {{{inst(Mult, dispatch(atts(left), Simplify), dispatch(atts(right), Simplify)))}}}
  *
  * @groupname api Core API
  * @groupname deltaHelpers DeltaHelpers
  * @groupname context Context
  * @groupname types Parameterized Types
  * @groupname dependency External Dependencies
  * @groupname inst Instantiating data types
  * @groupdesc dependency Depends upon BaseDomain (for the core logic) and the desired
  * @groupdesc api Fundamental abstractions needed for any language-based solution to EP
  * @groupprio api 0
  * @groupdesc types Each language must define relevant abstractions that map to these types.
  *            It is acceptable if the same structure is used for multiple types (as an example,
  *            review CPPElement)
  * @groupprio types 10
  * @groupdesc context Each language and approach needs different solutions to assemble the logic
  *           for a given (data-type and operation). The top-level concepts are shown here.
  * @groupprio types 20
  * @groupdesc inst When generating test cases, it is essential to include construction
  *            code that constructs instances of the data types. In addition, some receursive
  *            operations depend on being able to constrct instances of data types.
  * @groupprio context 30
  * @groupdesc deltaHelpers When weaving together code expressions representing partial fragments
  *           for a given logic, these helper methods are useful in capturing the desired structures.
  * @groupprio deltaHelpers 40
  */
abstract class LanguageIndependentGenerator(val evolution:Evolution, val names: NameProvider) {
  /**
    * Base concept for the representation of program unit on disk.
    * @group types
    */
  type CompilationUnit

  /**
    * Base concept for a single expression in the language.
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
    * Base concept to represent a block of code with result values.
    *
    * This separates out the resulting expressions from the block of statements (and they don't have to show up
    * in the code block).
    * @group inst
    */
  trait CodeBlockWithResultingExpressions {
    def block: Seq[Statement]
    def resultingExpressions: Seq[Expression]

    /** Appends a code block that depends on the results of this code block */
    def appendDependent(other: Seq[Expression] => CodeBlockWithResultingExpressions): CodeBlockWithResultingExpressions = {
      val nextBlock = other(resultingExpressions)
      val allStatements = block ++ nextBlock.block
      new CodeBlockWithResultingExpressions {
        def block: Seq[Statement] =  allStatements
        def resultingExpressions: Seq[Expression] = nextBlock.resultingExpressions
      }
    }

    /** Appends an independent code block and concatenate its results */
    def appendIndependent(other: CodeBlockWithResultingExpressions): CodeBlockWithResultingExpressions = {
      val allStatements = block ++ other.block
      val allResults = resultingExpressions ++ other.resultingExpressions
      new CodeBlockWithResultingExpressions {
        def block: Seq[Statement] =  allStatements
        def resultingExpressions: Seq[Expression] = allResults
      }
    }
  }

  /**
    * Helpers to construct code blocks with result expressions
    * @group inst
    */
  object CodeBlockWithResultingExpressions {
    val empty: CodeBlockWithResultingExpressions = apply()
    def apply(resultExps: Expression*): CodeBlockWithResultingExpressions =
      new CodeBlockWithResultingExpressions {
        def block: Seq[Statement] = Seq.empty
        def resultingExpressions: Seq[Expression] = resultExps
      }

    def apply(stmts: Statement*)(resultExps: Expression*): CodeBlockWithResultingExpressions =
      new CodeBlockWithResultingExpressions {
        def block: Seq[Statement] = stmts
        def resultingExpressions: Seq[Expression] = resultExps
      }
  }


  /**
    * For the processed model, return generated code artifacts for solution.
    * @group api
    */
  def generatedCode: Seq[CompilationUnit]

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated language-specific type associated with the abstract type.
    *
    * By throwing a runtime exception, this method terminates any code generation which fails
    * to account for a given type in the model.
    *
    * Note that the top-most type is ''domain.baseTypeRep''. If no converter is defined for the
    * given type, then this method will ultimately throw a ''scala.NotImplementedError'' exception.
    *
    * Each language-specific extension must determine how to handle Void as a type.
    *
    * @param tpe    type from model to be converted into language-specific type.
    * @group api
    */
  @throws[NotImplementedError]("if given type has no resolution.")
  def tpe(tpe: TypeRep) : Type = {
    throw new scala.NotImplementedError(s"""No rule to compile type "$tpe".""")
  }

  /**
    * Given a data type (and potential arguments) returns an expression type that instantiates the data type.
    *
    * @param tpeCase      desired DataType subtype
    * @param params    potential parameters
    * @return          code fragment suitable for instantiating data type
    * @group inst
    */
  def instantiate(tpeCase: DataTypeCase, params:Expression*): CodeBlockWithResultingExpressions

  /**
    * Convert a scala expression into the target language.
    *
    * The ExistsInstance could be a primitive type (Double, String, int) and if a domain instance, the request
    * is delegated to toTargetLanguage(domain.Inst)
    *
    * @param inst   the ExistsInstance captures a type (TypeRep) and an instance which varies based on trait.
    * @return             code fragment that converts values from Meta-Model test cases into actual
    *                     language-specific values.
    * @group inst
    */
  def instantiate(inst: InstanceRep) : CodeBlockWithResultingExpressions = {
    inst.inst match {
      case domInst: DataTypeInstance => instantiate(domInst)
      case _ => throw new scala.NotImplementedError(s"No rule to compile instantiations of ${inst.tpe}.")
    }
  }

  /**
    * Convert a domain specific data type instance into the target language.
    *
    * @param inst    desired instance for which a constructing code fragment is returned.
    * @group inst
    */
  def instantiate(inst: DataTypeInstance): CodeBlockWithResultingExpressions = {
    val attributeInstantiation =
      inst.attributeInstances.foldLeft(CodeBlockWithResultingExpressions.empty) {
        case (block, attributeInstance) => block.appendIndependent(instantiate(attributeInstance))
      }
    attributeInstantiation.appendDependent(attributeInstances =>
      instantiate(inst.tpeCase, attributeInstances: _*)
    )
  }

  /**
    * Return an expression that refers to the given sub-structure of a data-type by a
    * specific attribute.
    *
    *
    * By throwing a runtime exception, this method terminates any code generation that
    * refers to an invalid attribute by mistake.
    *
    * @param tpeCase   desired data-type
    * @param att   desired Attribute
    * @group api
    */
  @throws[scala.NotImplementedError]("If given attribute doesn't exist in data-type.")
  def accessAttribute(tpeCase:DataTypeCase, att:Attribute) : Expression = {
    throw new scala.NotImplementedError(s"""No rule to compile access to attribute "${att.name}" for "${tpeCase.name}.""")
  }

  /**
    * For all possible EP solutions, this method generates the sequence of statements that result
    * for a given operation and data-type.
    *
    * Must be return a sequence of statements since some operations require a more substantial
    * implementation depending upon the programming language.
    *
    * Must be Statements (rather than just an Expression) because in most operations, a value of
    * some sort is returned, thus instead of just "expr" it becomes "return expr;" To activate the
    * "return expr;" statement, use the [[toOperationResult]] method.
    *
    * @param tpeCase    data-type for the context
    * @param op     operation for the context
    * @group api
    */
  @throws[scala.NotImplementedError]("If no (data-type, operation) combination defined.")
  def logic(tpeCase:DataTypeCase, op:Operation) : Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for type case "${tpeCase.name}" """)
  }

  /**
    * Logic produces the sequence of statements that encodes the logic of an operation on a data-type.
    * As part of those statements, there is often a computed value that represents the result
    * of the operation. In some languages, this result needs to be returned (i.e., Java or C++); in
    * some EP approaches, this result is simply stored (i.e., see C++ implementation of visitorTable).
    * In other languages (i.e., Scala or functional) there is no specific return statement.
    *
    * In any event, each approach determines how to process the result. It could be handled in
    * language-specific manner and then overridden as needed by the EP approach.
    *
    * @param expr   Expression that represents the result of a log(exp, op).
    * @group api
    */
  def toOperationResult (expr:Expression) : Seq[Statement]

  // TODO: cleanup dispatch, naming conventions and parameter duplication in and Delta, Source

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
  def dispatch(expr: Expression, op: Operation, params: Expression*) : Expression

  /**
    * The '''logic(exp,op)''' that represents the code statements for applying a given operation
    * on a specific data-type can be defined using sub-expressions that represent the dispatch
    * of that operation (or a new operation) on a different context (or the same context).
    *
    * The three cases that are covered include:
    *
    *   1. [[dispatchChild]] : a new operation must be dispatched to a child sub-expression.
    *   1. [[dispatchSelf]]  : a new operation must be dispatched to self.
    *   1. [[dispatchToExpression]]  : a new (or same) operation must be dispatched to a new context expression
    *
    * All of these options are handled by this interface. Some of the more complicated
    * EP approaches require complicated implementations of this method.
    *
    * The default implementation throws an exception if ''delta.expr'' is empty (since it has
    * no way to know what the current context is). Individual EP generators must handle this
    * case on their own). When there is a ''delta.expr'' to use, then this default implementation
    * covers the following two cases:
    *
    *   1. if ''delta.op'' is defined, this becomes a '''dispatch(delta.expr, delta.op)'''
    *   1. if ''delta.op'' is not defined, then this becomes a '''dispatch(delta.expr, source.op)'''
    *
    * @param source     The source context where dispatch occurs
    * @param delta      The delta context that describes desired expression and operation
    * @group api
    */
  def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      throw new scala.NotImplementedError(s""" Self case must be handled by individual generators. """)
    } else {
      if (delta.op.isDefined) {
        dispatch(delta.expr.get, delta.op.get, delta.params: _*)
      } else {
        dispatch(delta.expr.get, source.op.get, delta.params: _*)
      }
    }
  }

  /**
    * A Context represents the situation in EP where the logic for a given data-type and
    * operation is generated. It can be considered as "Subject Verb Object*" with a number
    * of objects as potential parameters.
    *
    * @param exp      (optional) data-type of the context.
    * @param op       (optional) operation.
    * @param params   (optional variable length) parameters to the operation as code expressions.
    * @group context
    */
  abstract class Context(
    val exp: Option[DataTypeCase],
    val op: Option[Operation],
    val params: Expression*
  )

  /**
    * When code is being generated independently (either within a test case or perhaps even
    * within the logic for a specific data-type,operation pair) use the [[NoSource]] source
    * context to specify a break from the existing context.
    *
    * @group context
    */
  case object NoSource extends Context(None, None)

  /**
    * Point in expression problem solution where logic is to be inserted for a given data-type, e,
    * and operation, o, with given optional parameters, p.
    *
    * @param e    data-type of interest.
    * @param o    operation being performed on the data-type.
    * @param p    optional variable length parameters for this operation as code expressions.
    * @group context
    */
  case class Source(e:DataTypeCase, o:Operation, p:Expression*) extends Context(Some(e), Some(o), p : _*)

  /**
    * The logic for a given source context will typically need to weave together code fragments
    * from different contexts, referred to as [[Delta]].
    *
    * @param expr     (optional) code fragment representing expression derived from current context.
    * @param op       (optional) operation.
    * @param p        (optional variable length) parameters to the operation as code expressions.
    * @group context
    */
  class Delta(val expr:Option[Expression], override val op:Option[Operation], p:Expression*) extends Context(None, op, p : _*)

  /**
    * Helper method for creating a [[Delta]] context that represents a new operation (with
    * potential parameters) being applied to a child of the current source context (identified
    * by its attribute).
    *
    * This method uses [[accessAttribute]] on the source to generate the code fragment representing
    * the expression to use within the returned [[Delta]]
    *
    * @param att       child element, identified by actual attribute.
    * @param op        operation to perform on the child element.
    * @param params    optional variable length parameters for operation as code expressions.
    * @group deltaHelpers
    */
  def dispatchChild(exp:DataTypeCase, att:Attribute, op:Operation, params:Expression*) : Delta = {
    dispatchToExpression(accessAttribute(exp, att), op, params : _ *)
  }

  /**
    * Helper method for creating a [[Delta]] context that represents a new operation (with
    * potential parameters) being applied to a new expression context, passed in as 'expr'
    * parameter.
    *
    * If source operation is undefined, then this method returns a new independent
    *
    * @param expr      code fragment that will provide part of the new context.
    * @param op        designated operation on this new context.
    * @param params    optional variable length parameters for this operation as code expressions.
    * @group deltaHelpers
    */
  def dispatchToExpression(expr:Expression, op:Operation, params:Expression*) : Delta = {
    new Delta(Some(expr), Some(op), params: _*)
  }

  /**
    * Helper method for constructor Delta when requesting operation on self.
    *
    * Since the context of source remains the same, the [[Delta]] returned only
    * records the new operation,op, with its potential parameters.
    *
    * NOTE: POTENTIAL INFINITE LOOP IF CALL deltaSelfOp on self with same operation
    *
    * @param op        desired operation.
    * @param params    optional variable length parameters of this operation as code expressions.
    * @group deltaHelpers
    */
  def dispatchSelf(op: Operation, params:Expression*) : Delta = {
    new Delta(None, Some(op), params : _*)
  }
}
