package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j.AbstractGenerator
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AlgebraGenerator extends AbstractGenerator {
  val domain:BaseDomain with ModelDomain

  /**
    * Must eliminate any operation that returns E as value, since Algebra doesn't instantiate the intermediate structures
    */
  override def compatible(model:domain.Model):domain.Model = {
    if (model.isEmpty) { return model }

    // rebuild by filtering out all operations that return Exp.
    domain.Model(model.name, model.types,
      model.ops.filterNot(op => op.returnType.isDefined && op.returnType.get.equals(domain.baseTypeRep)),
      compatible(model.last))
  }

  override def getJavaClass : Expression = {
    Java(s"getClass()").expression[Expression]()
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    * For interpreter, we use a factory method that has been placed in the class, and that allows
    * the very specialized types to be used.
    */
  override def inst(exp:domain.Atomic)(op:domain.Operation)(params:Expression*): Expression = {
    Java(exp.name + "(" + params.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"${att.name}").expression[Expression]()).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def recurseOn(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.${op.name}($args)""").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => Java("E").tpe()
      case _ => Java ("void").tpe()  // reasonable stop
    }
  }

  /** Return designated Exp type with replacement. */
  def recursiveTypeGenerator(tpe:domain.TypeRep, replacement:Type) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => replacement
      case _ => typeConverter(tpe)
    }
  }

  /**
    * Every operation gets a class whose implementation contains method implementations for
    * all known operations. This class extends the most recently defined class for the
    * same operation (should one exist).
    */
  def operationGenerator(model:domain.Model, op:domain.Operation): CompilationUnit = {

    // this gets "eval" and we want the name of the Interface.
    val name = op.name
    val returnType = typeConverter(op.returnType.get)
    val opType = Java(op.name.capitalize).tpe()
    var targetModel:domain.Model = null
    var fullName:String = null

    /** Computes previous ExpAlgebra class directly from the model. There are four distinct subcases. */
    val previous:String = if (model.ops.contains(op) || model.lastModelWithOperation().isEmpty) {
      targetModel = model.flat()
      if (model.types.isEmpty) {
        fullName = model.lastModelWithDataTypes().types.sortWith(_.name < _.name).map(exp => exp.name.capitalize).mkString("")
      } else {
        fullName = model.types.sortWith(_.name < _.name).map(exp => exp.name.capitalize).mkString("")
      }

      ""
    } else {
      if (model.types.isEmpty) {
        targetModel = model.lastModelWithDataTypes()
      } else {
        targetModel = model
      }
      fullName = targetModel.types.sortWith(_.name < _.name).map(exp => exp.name.capitalize).mkString("")

      // this is different! It may be that there are NO types for the lastOperationDefined, in which case we must go
      // back further to find one where there were types defined, and then work with that one
      val bestModel:domain.Model = if (model.lastModelWithOperation().types.nonEmpty) {
        model.lastModelWithOperation()
      } else {
        model.lastModelWithOperation().lastModelWithDataTypes()
      }

      s"extends ${name.capitalize}" +
        bestModel.types.sortWith(_.name < _.name)
          .map(op => op.name.capitalize).mkString("") + "ExpAlg"
    }

    val methods = targetModel.types.map(exp => {  // exp is either 'lit' or 'add'

      val subName = exp.name.toLowerCase   // to get proper etiquette for method names
      val code:Seq[Statement] = logic(exp)(op)
      val signatures = code.mkString("\n")

      val params:Seq[String] = exp.attributes.map(att => s"final ${recursiveTypeGenerator(att.tpe, opType)} ${att.name}")
      // creates method body
      val paramList = params.mkString(",")

      s"""
         |public ${name.capitalize} $subName($paramList) {
         |        return new ${name.capitalize}() {
         |            public $returnType $name() {
         |                $signatures
         |            }
         |        };
         |    }
           """.stripMargin
    }).mkString("\n")

    val str:String = s"""|package algebra;
                         |public class ${name.capitalize}${fullName}ExpAlg $previous implements ${fullName}ExpAlg<${name.capitalize}> {
                         |     $methods
                         |}
                         |""".stripMargin
    Java(str).compilationUnit()
  }

  /** Generate interface for an operation. */
  def baseInterface(op:domain.Operation) : CompilationUnit = {
    var signatures:Seq[String] = Seq.empty

    val name = op.name.toLowerCase
    val tpe = typeConverter(op.returnType.get)

    signatures = signatures :+ s"  $tpe $name();"

    //implementations
    val str = s"""|package algebra;
                  |interface ${op.name.capitalize} {
                  |  ${signatures.mkString("\n")}
                  |}""".stripMargin
    Java(str).compilationUnit()
  }

  /**
    * As ExpAlg is refined, must add new datatypes and be sure to extend most recently defined
    * Interface for the algebra.
    *
    * Produces ever-expanding ladder of interface definitions for the *ExpAlg
    *
    * Only call when model.types is non-empty
    */
  def extendedInterface(model:domain.Model) : CompilationUnit = {

    // must be based on the new dataTypes being defined in model (if none, then why here? return)
    val types:Seq[String] = model.types.sortWith(_.name < _.name).map(exp => exp.name)
    val newName = types.mkString("")

    val signatures = model.types.map(exp => {
        val subName = exp.name.toLowerCase

        val params: Seq[String] = exp.attributes
          .map(att => s"final ${recursiveTypeGenerator(att.tpe, typeConverter(att.tpe))} ${att.name}")

        // creates method signature from parameters
        val paramList = params.mkString(",")

        s"""E $subName($paramList);"""

      }).mkString("\n")

    // when extending the first one, stop at just ExpAlg
    val previous: String = if (model.last.lastModelWithDataTypes().isEmpty) {
      ""
    } else {
      "extends " + model.last.lastModelWithDataTypes().types
        .sortWith(_.name < _.name).map(exp => exp.name.capitalize).mkString("")
        .concat("ExpAlg<E>")
    }

    Java(s"""|package algebra;
             |interface ${newName}ExpAlg<E> $previous {
             | $signatures
             |}
             |""".stripMargin).compilationUnit()
  }


  /** Starting from oldest (base) model, work forward in history. */
  def processModel(models:Seq[domain.Model]): Seq[CompilationUnit] = {

    // each one is handled individually, then by going backwards, we can find out where the base is
    // ans work outwards from there. Note we should likely foldLeft to create a sequence, which is the
    // reverse of the history.
    var comps: Seq[CompilationUnit] = Seq.empty
    var operations:Seq[domain.Operation] = Seq.empty

    models.foreach(model => {

      // one of these two conditions MUST be true. Either define new interface for each operation
      // or extend existing *ExpAlg interface
      if (model.ops.nonEmpty) {
        model.ops.foreach(op => {
          comps = comps :+ baseInterface (op)
          comps = comps :+ operationGenerator(model, op)
        })
      }

      if (model.types.nonEmpty) {
        comps = comps :+ extendedInterface(model)
      }

      // maintain increasing collection of operations. As new operations are defined, one must
      // create methods for each existing data type from the past
      operations = operations ++ model.ops

      // If no new operation defined, but new data types, must deal with extensions.
      if (model.types.nonEmpty && model.ops.isEmpty) {
        operations.foreach(op => {
          comps = comps :+ operationGenerator(model, op)
        })
      }
    })

    comps
  }
}
