package example.expression.algebra    /*DI:LD:AD*/

import com.github.javaparser.ast.body.{BodyDeclaration, MethodDeclaration}
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j.{JavaBinaryMethod, JavaGenerator, StandardJavaBinaryMethod}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AlgebraGenerator extends JavaGenerator with JavaBinaryMethod with StandardJavaBinaryMethod {
  val domain:BaseDomain with ModelDomain

  def useLambdaWherePossible: Boolean = true

  /**
    * Must eliminate any operation that returns E as value, since Algebra doesn't instantiate the intermediate structures
    */
  def process(model:domain.Model):domain.Model = {
    if (model.isEmpty) { return model }

    // rebuild by filtering out all operations that return Exp.
    domain.Model(model.name, model.types,
      model.ops.filterNot(op => op.isInstanceOf[domain.ProducerOperation]),
      process(model.last))
  }

  /**
    * Responsible for delegating to a new operation on the current data-type context identified
    * by exp.
    */
  override def delegateFixMe(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val m:domain.Model = getModel.findType(exp)
    val fullName = m.types.sortWith(_.name < _.name).map(exp => exp.name.capitalize).mkString("")

    // args can all be NULL since we are not concerned with children
    val args = exp.attributes.map(att => "null").mkString(",")
    val opargs = params.mkString(",")

    Java(s"new ${op.name.capitalize}$fullName${domain.baseTypeRep.name}Alg().${exp.name.toLowerCase}($args).${op.name.toLowerCase}($opargs)").expression[Expression]()
  }

  /** For Algebra Generator, same behavior as delegate. */
  override def identify(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    delegateFixMe(exp, op, params : _*)
  }

  /**
    * Generating an algebra solution requires processing the models in chronological ordering to be able
    * to prepare the proper interfaces
    *
    * @return
    */
  override def generatedCode():Seq[CompilationUnit] = {
    val model = process(getModel)

    //  binary methods for helper
    val decls:Seq[CompilationUnit] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ processModel(model.inChronologicalOrder)
  }


  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    * Note: This capability is preliminary and not yet ready for use.
    */
//  def inst(exp:domain.Atomic)(params:Expression*): Expression = {
//    Java(exp.name + "(" + params.map(expr => expr.toString()).mkString(",") + ")").expression()
//  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"${att.name}").expression[Expression]()).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
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
      targetModel = model.flatten()
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
      val bestModel: domain.Model = if (targetModel.equals(model)) {
        targetModel.last.lastModelWithDataTypes()
      } else {
        if (model.lastModelWithOperation().types.nonEmpty) {
          model.lastModelWithOperation()
        } else {
          model.lastModelWithOperation().lastModelWithDataTypes()
        }
      }

      s"extends ${name.capitalize}" +
        bestModel.types.sortWith(_.name < _.name)
          .map(op => op.name.capitalize).mkString("") + s"${domain.baseTypeRep.name}Alg"
    }

   // Handle binary methods...
    val op_params = op match {
      case bm:domain.BinaryMethod => binaryMethodParameters(op, typeConverter)
      case _ => parameters(op)
    }

    val methods = targetModel.types.flatMap(exp => {  // exp is either 'lit' or 'add'

      val subName = exp.name.toLowerCase   // to get proper etiquette for method names
      val code:Seq[Statement] = logic(exp, op)
      val signatures = code.mkString("\n")

      // handle covariant typing locally
      val params = exp.attributes.map(att =>
        att.tpe match {
          case domain.baseTypeRep => s"final $opType ${att.name}"
          case _ =>   s"final ${typeConverter(att.tpe)} ${att.name}"
        })

      // creates method body
      val paramList = params.mkString(",")

      val helpers:Seq[BodyDeclaration[_]] = op match {
        case bm:domain.BinaryMethod =>
          logicAsTree(exp)
        case _ => Seq.empty
      }

      val str = if (helpers.isEmpty && useLambdaWherePossible) {
        Java(s"""
                |public ${name.capitalize} $subName($paramList) {
                |   return ($op_params) -> {$signatures };
                |}""".stripMargin)
      } else {
        Java(s"""
                |public ${name.capitalize} $subName($paramList) {
                |  return new ${name.capitalize}() {
                |    ${helpers.mkString("\n")}
                |    public $returnType $name($op_params) {
                |        $signatures
                |    }
                |  };
                |}""".stripMargin)
      }

      str.methodDeclarations
    })

    val delegate:Seq[BodyDeclaration[_]] = op match {
      case bm:domain.BinaryMethod =>
        Java(s"""${domain.AsTree.name.capitalize}$fullName${domain.baseTypeRep.name}Alg asTree = new ${domain.AsTree.name.capitalize}$fullName${domain.baseTypeRep.name}Alg();""").classBodyDeclarations()
      case _ => Seq.empty
    }

    val str:String = s"""|package algebra;
                         |public class ${name.capitalize}$fullName${domain.baseTypeRep.name}Alg $previous implements $fullName${domain.baseTypeRep.name}Alg<${name.capitalize}> {
                         |     ${delegate.mkString("\n")}
                         |
                         |     ${methods.mkString("\n")}
                         |}""".stripMargin
    Java(str).compilationUnit()
  }

  /** Generate interface for an operation. */
  def baseInterface(op:domain.Operation) : CompilationUnit = {
    var signatures:Seq[String] = Seq.empty

    val name = op.name.toLowerCase
    val tpe = typeConverter(op.returnType.get)

    val params = op match {
      case bm:domain.BinaryMethod => binaryMethodParameters(op, typeConverter)
      case _ => parameters(op)
    }

    signatures = signatures :+ s"  $tpe $name($params);"

    val parent:String = op match {
      case b:domain.BinaryMethod => s"extends ${domain.AsTree.name.capitalize}"
      case _ => ""
    }

    // implementations
    val str = s"""|package algebra;
                  |interface ${op.name.capitalize} $parent {
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
          .map(att => s"final ${typeConverter(att.tpe)} ${att.name}")

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
        .concat(s"${domain.baseTypeRep.name}Alg<E>")
    }

    Java(s"""|package algebra;
             |interface $newName${domain.baseTypeRep.name}Alg<E> $previous {
             | $signatures
             |}
             |""".stripMargin).compilationUnit()
  }


  /** Starting from oldest (base) model, work forward in history. */
  def processModel(models:Seq[domain.Model]): Seq[CompilationUnit] = {

    // each one is handled individually, then by going backwards, we can find out where the base is
    // ans work outwards from there.
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

      // If new data types defined, must deal with extensions.
      if (model.types.nonEmpty) {
        comps = comps :+ extendedInterface(model)
        operations.foreach(op => {
          comps = comps :+ operationGenerator(model, op)
        })
      }

      // maintain increasing collection of operations. As new operations are defined, one must
      // create methods for each existing data type from the past
      operations = operations ++ model.ops
    })

    comps
  }
}
