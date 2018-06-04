package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import example.expression.j.AbstractGenerator
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AlgebraGenerator extends AbstractGenerator {
  val domain:Domain
  import domain._

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:expressions.Exp) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"e.get${att.name.capitalize}()").expression[Expression]()).toMap
//    exp.attributes.map(att => Java(s"e.get${att.name.capitalize}()").expression[Expression]())
  }

  /** Directly access local method, one per operation. */
  override def recurseOn(expr:Expression, op:Operation) : Expression = {
    Java(s"""$expr.accept(new ${op.name.capitalize}())""").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case types.Exp => Java("Exp").tpe()
      case _ => Java ("void").tpe()  // reasonable stop
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type. */
  def generateBase(model:Model): CompilationUnit = {
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

  /** Each sub-type has an accept method as required by visitor pattern */
  def visitorMethodGenerator(): MethodDeclaration = {
    Java (s"""
             |public <R> R accept(Visitor<R> v) {
             |   return v.visit(this);
             |}""".stripMargin).methodDeclarations().head
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:expressions.Exp)(op:Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType visit(${exp.name} e) {
             |  ${methodBodyGenerator(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
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

  def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }


  /** Generate the full class for the given expression sub-type. */
  def generateExp(domain:Model, e:expressions.Exp) : CompilationUnit = {
    val name = e.toString

    // val methods:Seq[MethodDeclaration] = domain.ops.map(methodGenerator(e))
    val visitor:MethodDeclaration = visitorMethodGenerator()
    val atts:Seq[FieldDeclaration] = e.attributes.flatMap(att => Java(s"private ${typeGenerator(att.tpe)} ${att.name};").fieldDeclarations())

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty
    var methods:Seq[MethodDeclaration] = Seq.empty

    val fields:Seq[FieldDeclaration] = e.attributes.map(att => {
      val capAtt = att.name.capitalize
      val tpe = typeGenerator(att.tpe)

      params = params :+ s"$tpe ${att.name}"
      cons   = cons   :+ s"  this.${att.name} = ${att.name};"

      // make the set/get methods
      methods = methods ++ Java(s"""
                                   |public $tpe get$capAtt() { return ${att.name};}
                                   |public void set$capAtt($tpe val) { this.${att.name} = val; }
                                """.stripMargin).methodDeclarations()

      Java(s"private $tpe ${att.name};").fieldDeclarations().head
    })

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

}







