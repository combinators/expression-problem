package example.expression.oo

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
trait StraightGenerator extends AbstractGenerator with DataTypeSubclassGenerator with OperationAsMethodGenerator {
  val domain:BaseDomain with ModelDomain

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"${att.name}").expression[Expression]()).toMap
  }

  /** Retrieve Java Class associated with given context. Needed for operations with Exp as parameter. */
  override def getJavaClass() : Expression = {
    Java(s"getClass()").expression[Expression]()
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def recurseOn(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.${op.name}($args)""").expression()
  }

  /** Return designated Java type associated with type. */
  override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => Java("Exp").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.TypeRep = tuple._2

      typeConverter(tpe).toString + " " + name
    }).mkString(",")
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:domain.Atomic)(op:domain.Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }

    val params = parameters(op)
    Java(s"""|public $retType ${op.name}($params) {
             |  ${logic(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:domain.Model, exp:domain.Atomic) : CompilationUnit = {
    val name = exp.toString

    val methods:Seq[MethodDeclaration] = model.ops.map(methodGenerator(exp))
    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${typeConverter(att.tpe)} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att => s"${typeConverter(att.tpe)} ${att.name}")
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    val constructor = Java(s"""|public $name (${params.mkString(",")}) {
                               |   ${cons.mkString("\n")}
                               |}""".stripMargin).constructors().head

    Java(s"""|package oo;
             |public class $name extends Exp {
             |  ${constructor.toString}
             |  ${atts.mkString("\n")}
             |  ${methods.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:domain.Model): CompilationUnit = {
    val signatures: Seq[MethodDeclaration] = model.ops.flatMap(op => {

        val retType = op.returnType match {
          case Some(tpe) => typeConverter(tpe)
          case _ => Java("void").tpe
        }

      val params = parameters(op)
      Java(s"public abstract $retType ${op.name}($params);").methodDeclarations()
    })

    Java(s"""|package oo;
             |public abstract class Exp {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }
}
