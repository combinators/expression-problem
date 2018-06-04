package example.expression.j

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body. MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AbstractGenerator {
  val domain:Domain
  import domain._

  /** Determines the Java expression for all children of a Exp subtype. */
  def subExpressions(exp:expressions.Exp) : Map[String, Expression]

  /** Responsible for dispatching sub-expressions. */
  def recurseOn(expr:Expression, op:Operation) : Expression

  /** Return designated Java type associated with type, or void if all else fails. */
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:expressions.Exp)(op:Operation): MethodDeclaration

  /** Generates the sequence of statements for any implementation of an expression sub-type. */
  def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement]

  /** Generate the full class for the given expression sub-type. */
  def generateExp(domain:Model, e:expressions.Exp) : CompilationUnit

  /** Generate the base class. */
  def generateBase(domain:Model): CompilationUnit
}







