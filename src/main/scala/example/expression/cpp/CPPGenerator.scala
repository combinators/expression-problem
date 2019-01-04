package example.expression.cpp        /*DI:LD:AI*/

import example.expression.generator.LanguageIndependentGenerator

/**
  * Any Haskell EP approach can extend this Generator
  */
trait CPPGenerator extends LanguageIndependentGenerator {

  type CompilationUnit = CPPFile
  type Type = CPPType
  type Expression = CPPElement
  type Statement = CPPElement

  /** Default helper to convert string into Expression. */
  def expression(s:String) : Expression = new CPPElement(s)

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
   def dependency(op: domain.Operation): scala.List[domain.Operation] = List.empty

  /**
    * Responsible for delegating to a new operation on the current context.
    */
  def delegate(exp:domain.Atomic, op:domain.Operation, params:CPPElement*) : CPPElement = {
    new CPPElement("CppReplaceMe")
  }

}
