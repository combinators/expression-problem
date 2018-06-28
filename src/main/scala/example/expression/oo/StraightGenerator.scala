package example.expression.oo

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j.{AbstractGenerator, DataTypeSubclassGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait StraightGenerator extends AbstractGenerator with DataTypeSubclassGenerator {
  val domain:BaseDomain with ModelDomain

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:domain.expressions.Exp) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"${att.name}").expression[Expression]()).toMap
  }

//  /** Directly access local method, one per operation. */
//  override def recurseOn(expr:Expression, op:domain.Operation) : Expression = {
//    Java(s"""$expr.${op.name}()""").expression()
//  }

  /** Directly access local method, one per operation, with a parameter. */
  override def recurseOn(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.${op.name}($args)""").expression()
  }

  /** Return designated Java type associated with type. */
  def typeGenerator(tpe:domain.types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Exp => Java("Exp").tpe()
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:domain.expressions.Exp)(op:domain.Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    val params:String = op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.types.Types = tuple._2

      typeGenerator(tpe).toString + " " + name
    }).mkString(",")


    Java(s"""|public $retType ${op.name}($params) {
             |  ${methodBodyGenerator(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  /** Throws run-time exception to catch when an operation/exp pair is missing. */
  def methodBodyGenerator(exp:domain.expressions.Exp)(op:domain.Operation): Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.getClass.getSimpleName}" """)
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:domain.Model, exp:domain.expressions.Exp) : CompilationUnit = {
    val name = exp.toString

    val methods:Seq[MethodDeclaration] = model.ops.map(methodGenerator(exp))
    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${typeGenerator(att.tpe)} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att => s"${typeGenerator(att.tpe)} ${att.name}")
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    val constructor = Java(s"""|public $name (${params.mkString(",")}) {
                               |   ${cons.mkString("\n")}
                               |}""".stripMargin).constructors().head

    Java(s"""|package oo;
             |public class $name extends Exp {
             |
             |  ${constructor.toString}
             |
             |  ${atts.mkString("\n")}
             |
             |  ${methods.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:domain.Model): CompilationUnit = {
    val signatures: Seq[MethodDeclaration] = model.ops.flatMap(op => {
      // Allow for operations to be void; if not an issue in future, then filter out here...
        val retType = op.returnType match {
          case Some(tpe) => typeGenerator(tpe)
          case _ => Java("void").tpe
        }

        val params:String = op.parameters.map(tuple => {
          val name:String = tuple._1
          val tpe:domain.types.Types = tuple._2

          typeGenerator(tpe).toString + " " + name
        }).mkString(",")

      Java(s"public abstract $retType ${op.name}($params);").methodDeclarations()
      })

    // same every time
    Java(s"""|package oo;
             |public abstract class Exp {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }

}
