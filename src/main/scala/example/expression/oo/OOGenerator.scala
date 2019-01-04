package example.expression.oo  /*DI:LD:AD*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j._
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait OOGenerator extends JavaGenerator with DataTypeSubclassGenerator with StandardJavaBinaryMethod with OperationAsMethodGenerator with Producer with JavaBinaryMethod {

  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel:domain.Model

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. A Base class to be superclass of them all
    */
  def generatedCode():Seq[CompilationUnit] = {
    val flat = getModel.flatten()

    //  binary methods for helper
    val decls:Seq[CompilationUnit] = if (flat.ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      println ("added helpers")
      helperClasses()
    } else {
      println ("No helpers")
      Seq.empty
    }

    decls ++ flat.types.map(tpe => generateExp(flat, tpe)) :+      // one class for each sub-type
      generateBase(flat)                                           // base class $BASE

  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"${att.name}").expression[Expression]).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"$expr.${op.name}($args)").expression()
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:Atomic)(op:Operation): MethodDeclaration = {
    val params = parameters(op)
    Java(s"""|public ${returnType(op)} ${op.name}($params) {
             |  ${logic(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations.head
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:Model, exp:Atomic) : CompilationUnit = {
    val methods = model.ops.map(methodGenerator(exp))

    Java(s"""|package oo;
             |public class ${exp.toString} extends ${domain.baseTypeRep.name} {
             |  ${constructor(exp)}
             |  ${fields(exp).mkString("\n")}
             |  ${methods.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:Model): CompilationUnit = {
    val signatures = model.ops.flatMap(op => {
       Java(s"public abstract ${returnType(op)} " +
        s"${op.name}(${parameters(op)});").methodDeclarations
    })

    Java(s"""|package oo;
             |public abstract class ${domain.baseTypeRep.name} {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }
}
