package example.expression.scalaVisitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j.{AbstractGenerator, DataTypeSubclassGenerator, OperationAsMethodGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait VisitorGenerator extends AbstractGenerator with DataTypeSubclassGenerator with OperationAsMethodGenerator {
  val domain:BaseDomain with ModelDomain

  /** For visitor design solution, access through default 'e' parameter */
  override def subExpressions(exp:domain.expressions.Exp) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"e.get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  override def getJavaClass() : Expression = {
    Java(s"e.getClass()").expression[Expression]()
  }

//  /** Directly access local method, one per operation. */
//  override def recurseOn(expr:Expression, op:domain.Operation) : Expression = {
//    Java(s"""$expr.accept(new ${op.name.capitalize}())""").expression()
//  }

  /** Directly access local method, one per operation, with a parameter. */
  override def recurseOn(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.accept(new ${op.name.capitalize}($args))""").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeGenerator(tpe:domain.types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Exp => Java("Exp").tpe()
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type. */
  override def generateBase(model:domain.Model): CompilationUnit = {
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

  /** For visitor, the base class defines the accept method used by all subclasses. */
  def generateBaseClass():CompilationUnit = {
    Java(s"""|package expression;
             |
             |public abstract class Exp {
             |    public abstract <R> R accept(Visitor<R> v);
             |}
             |""".stripMargin).compilationUnit()
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  override def methodGenerator(exp:domain.expressions.Exp)(op:domain.Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType visit(${exp.name} e) {
             |  ${methodBodyGenerator(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

//  /** Provides Exception for unsupported operation, expression pair. */
//  override def methodBodyGenerator(exp:domain.expressions.Exp)(op:domain.Operation): Seq[Statement] = {
//    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
//  }

  /** Generate the full class for the given expression sub-type. */
  override def generateExp(model:domain.Model, exp:domain.expressions.Exp) : CompilationUnit = {
    val name = exp.toString

    val visitor:MethodDeclaration = Java (s"""|public <R> R accept(Visitor<R> v) {
                                              |   return v.visit(this);
                                              |}""".stripMargin).methodDeclarations().head
    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${typeGenerator(att.tpe)} ${att.name};").fieldDeclarations())

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
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
  def operationGenerator(model:domain.Model, op:domain.Operation): CompilationUnit = {

    val signatures = model.types.map(exp => methodGenerator(exp)(op)).mkString("\n")

    // if operation has parameters then must add to visitor as well
    val atts:Seq[FieldDeclaration] = op.parameters.flatMap(p => Java(s"${typeGenerator(p._2)} ${p._1};").fieldDeclarations())
    val params:Seq[String] = op.parameters.map(p => s"${typeGenerator(p._2)} ${p._1}")
    val cons:Seq[Statement] = op.parameters.flatMap(p => Java(s"  this.${p._1} = ${p._1};").statements())

    // only add constructor if visitor has a parameter
    val constructor = if (op.parameters.isEmpty) {
      ""
    } else {
      Java(
        s"""|public ${op.name.capitalize} (${params.mkString(",")}) {
            |   ${cons.mkString("\n")}

            |}""".
          stripMargin).constructors().head
    }

    val tpe = typeGenerator(op.returnType.get)
    val s = Java(s"""|package expression;
                     |public class ${op.name.capitalize} extends Visitor<$tpe>{
                     |  ${constructor.toString}
                     |
                     |  ${atts.mkString("\n")}
                     |  $signatures
                     |}""".stripMargin)

    s.compilationUnit()
  }

}
