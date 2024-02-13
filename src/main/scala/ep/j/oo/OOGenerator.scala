package ep.j.oo  /*DI:LD:AD*/

import com.github.javaparser.ast.body.MethodDeclaration
import ep.domain.{BaseDomain, ModelDomain}
import ep.j._
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait OOGenerator extends JavaGenerator with DataTypeSubclassGenerator with OperationAsMethodGenerator with JavaBinaryMethod {

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
    val decls:Seq[CompilationUnit] = if (flat.hasBinaryMethod()) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ flat.types.map(tpe => generateExp(flat, tpe, "oo")) :+      // one class for each sub-type
      generateBase(flat, "oo")                                           // base class $BASE

  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:Atomic, att:Attribute) : Expression = {
    Java(s"${att.instance}").expression[Expression]
  }

  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      val op = delta.op.get.instance
      Java(s"this.$op()").expression()
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"$expr.${op.instance}($args)").expression()
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:Atomic, op:Operation): MethodDeclaration = {
    val params = parameters(op)
    Java(s"""|public ${returnType(op)} ${op.instance}($params) {
             |  ${logic(exp, op).mkString("\n")}
             |}""".stripMargin).methodDeclarations.head
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:Model, exp:Atomic, pkgName:String) : CompilationUnit = {
    val methods = model.ops.map(op => methodGenerator(exp, op))

    Java(s"""|package $pkgName;
             |public class ${exp.toString} extends ${domain.baseTypeRep.name} {
             |  ${constructor(exp)}
             |  ${fields(exp).mkString("\n")}
             |  ${methods.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:Model, pkgName:String): CompilationUnit = {
    val signatures = model.ops.flatMap(op => {
       Java(s"public abstract ${returnType(op)} " +
        s"${op.instance}(${parameters(op)});").methodDeclarations
    })

    Java(s"""|package $pkgName;
             |public abstract class ${domain.baseTypeRep.name} {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }
}
