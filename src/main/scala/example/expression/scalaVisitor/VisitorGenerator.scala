package example.expression.scalaVisitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import example.expression.j.{AbstractGenerator, DataTypeSubclassGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait VisitorGenerator extends AbstractGenerator with DataTypeSubclassGenerator {
  val domain:Domain
  import domain._

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:expressions.Exp) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"e.get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  /** Directly access local method, one per operation. */
  override def recurseOn(expr:Expression, op:Operation) : Expression = {
    Java(s"""$expr.accept(new ${op.name.capitalize}())""").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case types.Exp => Java("Exp").tpe()
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type. */
  override def generateBase(model:Model): CompilationUnit = {
    val signatures = model.types
      .map(exp => s"public abstract R visit(${exp.name} exp);").mkString("\n")

    Java (s"""|package expression;
              |/*
              | * A concrete visitor describes a concrete operation on expressions. There is one visit
              | * method per type in the class hierarchy.
              | */
              |public abstract class Visitor<R> {
              |
                |$signatures
              |}""".stripMargin).compilationUnit()
  }

  def generateBaseClass():CompilationUnit = {
    Java(s"""|package expression;
             |
             |public abstract class Exp {
             |    public abstract <R> R accept(Visitor<R> v);
             |}
             |""".stripMargin).compilationUnit()
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  override def methodGenerator(exp:expressions.Exp)(op:Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType visit(${exp.name} e) {
             |  ${methodBodyGenerator(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  /** Provides Exception for unsupported operation, expression pair. */
  override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }

  /** Generate the full class for the given expression sub-type. */
  override def generateExp(domain:Model, exp:expressions.Exp) : CompilationUnit = {
    val name = exp.toString

    val visitor:MethodDeclaration = Java (s"""|public <R> R accept(Visitor<R> v) {
                                              |   return v.visit(this);
                                              |}""".stripMargin).methodDeclarations().head
    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${typeGenerator(att.tpe)} ${att.name};").fieldDeclarations())

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    //var params:Seq[String] = Seq.empty
    // var cons:Seq[String] = Seq.empty
    val methods:Seq[MethodDeclaration] = exp.attributes.flatMap(att => {
      val capAtt = att.name.capitalize
      val tpe = typeGenerator(att.tpe)
      Java(s"public $tpe get$capAtt() { return ${att.name};}").methodDeclarations()
    })

    val params:Seq[String] = exp.attributes.map(att => s"${typeGenerator(att.tpe)} ${att.name}")
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    val constructor = Java(s"""|public $name (${params.mkString(",")}) {
                               |   ${cons.mkString("\n")}
                               |}""".stripMargin).constructors().head

    // visitor methods for accepting operations
    // make accept method call and add to class
    Java(s"""|package expression;
             |public class $name extends Exp {
             |
             |  ${constructor.toString}
             |
             |  ${atts.mkString("\n")}
             |  ${methods.mkString("\n")}
             |  ${visitor.toString()}
             |}""".stripMargin).compilationUnit()
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  def operationGenerator(model:Model, op:Operation): CompilationUnit = {

    val signatures = model.types.map(exp => methodGenerator(exp)(op)).mkString("\n")

    val tpe = typeGenerator(op.returnType.get)
    val s = Java(s"""|package expression;
                     |public class ${op.name.capitalize} extends Visitor<$tpe>{
                     |  $signatures
                     |}""".stripMargin)

    s.compilationUnit()
  }

}







