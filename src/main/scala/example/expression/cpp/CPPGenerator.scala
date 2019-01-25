package example.expression.cpp        /*DI:LD:AI*/

import example.expression.generator.LanguageIndependentGenerator

/**
  * Any Haskell EP approach can extend this Generator
  */
trait CPPGenerator extends LanguageIndependentGenerator with DependentDispatch {

  type CompilationUnit = CPPFile
  type Type = CPPType
  type Expression = CPPElement
  type Statement = CPPElement

  /** Default helper to convert string into Expression. */
  def expression(s:String) : Expression = new CPPElement(s)

  /**
    * Default behavior in C++ is to return an expression value.
    */
  def result (expr:Expression) : Seq[Statement] = {
    Seq(new CPPElement(s"return $expr;"))
  }

  // TODO: FIX
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    new CPPElement(s""""replaceMe"""")
  }

  /**
    * Return just the expression.
    */
  def valueOf(expr:Expression, params:CPPElement*): CPPElement = {
    expr
    //new CPPElement(s"$expr")
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
   def dependency(op: domain.Operation): scala.List[domain.Operation] = List.empty

  // TODO: FIX ME
  /**
    * Responsible for delegating to a new operation on the current context.
    */
  def delegateFixMe(exp:domain.Atomic, op:domain.Operation, params:CPPElement*) : CPPElement = {
    new CPPElement("CppReplaceMe")
  }

  /** For C++, resort to generated enums */
  def identify(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    new CPPElement(s"DefinedSubtypes::${exp.name.capitalize}Subtype")
  }
}
