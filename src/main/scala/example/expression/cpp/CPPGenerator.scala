package example.expression.cpp        /*DI:LD:AI*/

import example.expression.generator.LanguageIndependentGenerator

/**
  * Any C++ EP approach can extend this Generator
  *

#!/bin/bash -x
g++ *.cpp  -I ../cpputest/include -L ../cpputest/cpputest_build/lib -lCppUTest -lCppUTestExt -std=c++11

  */
trait CPPGenerator extends LanguageIndependentGenerator {

  type CompilationUnit = CPPFile
  type Type = CPPType
  type Expression = CPPElement
  type Statement = CPPElement

  /**
    * Default behavior in C++ is to return an expression value.
    */
  def result (expr:Expression) : Seq[Statement] = {
    Seq(new CPPElement(s"return $expr;"))
  }

  /** Standard implementation relies on dependent dispatch. TODO: FIX */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      throw new scala.NotImplementedError(s""" Self case must be handled by subclass generator. """)
    } else {
      if (delta.op.isDefined) {
        dispatch(delta.expr.get, delta.op.get, delta.params: _*)
      } else {
        dispatch(delta.expr.get, source.op.get, delta.params: _*)
      }
    }
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
}
