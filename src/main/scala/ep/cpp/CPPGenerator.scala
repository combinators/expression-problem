package ep.cpp        /*DI:LD:AI*/

import ep.generator.LanguageIndependentGenerator

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

  /**
    * Return just the expression.
    */
  def valueOf(expr:Expression, params:CPPElement*): CPPElement = {
    expr
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
   def dependency(op: domain.Operation): scala.List[domain.Operation] = List.empty


  /** Compute parameter "name" comma-separated list from operation. */
  def arguments(op:domain.Operation) : String = {
    op.parameters.map(param => param.name).mkString(",")
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(param => typeConverter(param.tpe).toString + " " + param.name).mkString(",")
  }
}
