package example.expression.oo  /*DI:LD:AD*/

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j._
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait OOGenerator extends AbstractGenerator with JavaGenerator with DataTypeSubclassGenerator with OperationAsMethodGenerator with BinaryMethod {

  val domain:BaseDomain with ModelDomain

  def getModel:domain.Model

  /**
    * Process the model as necessary. One could either (a) remove data types or operations that are non-sensical
    * for the given approach; or (b) flatten the hierarchy.
    */
  override def getProcessedModel:domain.Model = getModel.flat()

  /**
    * Generating a straight OO solution requires:
    *
    * 1. A Class for every
    * @param model
    * @return
    */
  def generatedCode(model:domain.Model):Seq[CompilationUnit] = {
    model.types.map(tpe => generateExp(model, tpe)) :+     // one class for each sub-type
      generateBase(model)                                  // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"${att.name}").expression[Expression]()).toMap
  }

  /** Retrieve Java Class associated with given context. Needed for operations with Exp as parameter. */
  override def getJavaClass : Expression = {
    Java(s"getClass()").expression[Expression]()
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.${op.name}($args)""").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => covariantReplacement.getOrElse(Java("Exp").tpe())
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  override def methodGenerator(exp:domain.Atomic)(op:domain.Operation): MethodDeclaration = {
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

    Java(s"""|package oo;
             |public class $name extends Exp {
             |  ${constructor(exp)}
             |  ${fields(exp).mkString("\n")}
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
