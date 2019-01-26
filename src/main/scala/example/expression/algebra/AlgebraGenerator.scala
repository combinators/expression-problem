package example.expression.algebra    /*DI:LD:AD*/

import com.github.javaparser.ast.body.{BodyDeclaration, MethodDeclaration}
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j.{JavaBinaryMethod, JavaGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AlgebraGenerator extends JavaGenerator with JavaBinaryMethod {
  val domain:BaseDomain with ModelDomain

  def useLambdaWherePossible: Boolean = true

  /** Algebra has special decisions to make with contextDispatch in all cases. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (source.op.isDefined && delta.expr.isEmpty && source.op.get.isInstanceOf[domain.BinaryMethod]) {
      // used for BinaryMethods which have already created a method to match deltaop
      val deltaop = delta.op.get
      val args: String = delta.params.mkString(",")
      Java(s"""${deltaop.name}($args)""").expression()
    } else  if (delta.expr.isEmpty) {
      val exp:domain.Atomic = source.exp.get
      recreate(exp, delta.op.get)
    } else {
      val expr:Expression = delta.expr.get
      if (delta.op.isDefined) {
        val deltaop = delta.op.get
        val args: String = delta.params.mkString(",")

        if (source.op.isDefined && source.op.get.isInstanceOf[domain.ProducerOperation]) {
          val op:domain.Operation = source.op.get
          Java(s"""$expr.${op.name}().${deltaop.name}($args)""").expression() // TODO: Can we ever have args for the first operation?
        } else {
          Java(s"""$expr.${deltaop.name}($args)""").expression()
        }
      } else {
        dispatch(delta.expr.get, source.op.get, delta.params: _*)
      }
    }
  }

  /**
    * Responsible for delegating to a new operation on the current data-type context identified
    * by exp.
    *
    * Complicated in algebra since there is no concept of the current object; instead, this
    * 'recreates' the current state, using 'null' as the children, since only the top-level
    * is needed.
    */
  def recreate(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val m:domain.Model = getModel.lastModelWithDataTypes()  // getModel.findType(exp)
    val fullName = m.types.sortWith(_.name < _.name).map(exp => exp.name.capitalize).mkString("")

    // args can all be NULL since we are not concerned with children
    val args = exp.attributes.map(att =>"null").mkString(",")   // was "null"
    val opargs = params.mkString(",")

    Java(s"new ${op.name.capitalize}$fullName${domain.baseTypeRep.name}Alg().${exp.name.toLowerCase}($args).${op.name.toLowerCase}($opargs)").expression[Expression]()
  }

  /**
    * Generating an algebra solution requires processing the models in chronological ordering to be able
    * to prepare the proper interfaces
    *
    * @return
    */
  override def generatedCode():Seq[CompilationUnit] = {
    //val model = process(getModel)

    //  binary methods for helper
    val decls:Seq[CompilationUnit] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      helperClasses()
    } else {
      Seq.empty
    }

    //  producer helper classes
    val prods:Seq[CompilationUnit] = if (getModel.flatten().ops.exists {
      case bm: domain.ProducerOperation => true
      case _ => false
    }) {
      generateStraightOO()
    } else {
      Seq.empty
    }

    decls ++ prods ++ processModel(getModel.inChronologicalOrder)
  }


  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    * Note: This capability is preliminary and not yet ready for use.
    */
  override def inst(exp:domain.Atomic, params:Expression*): Expression = {
    Java("algebra." + exp.name.toLowerCase() + "(" + params.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"${att.name}").expression[Expression]()).toMap
  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpression(exp:domain.Atomic, name:String) : Expression = {
    exp.attributes.filter(att => att.name.equals(name)).map(att => Java(s"${att.name}").expression[Expression]()).head
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

  /** Generated Combined API. */
  def algebraAPI : Type = {
    Java(s"algebra.Combined${domain.baseTypeRep.name}Alg.Combined").tpe()
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

    // if this operation is a producer, then we need special constructor
    val producerConstructor = op match {
      case po: domain.ProducerOperation => Java(
        s"""
          |// Binary operations are passed in necessary algebra to work with
          |Combined${domain.baseTypeRep.name}Alg algebra;
          |public ${name.capitalize}$fullName${domain.baseTypeRep.name}Alg(Combined${domain.baseTypeRep.name}Alg alg) {
          |		this.algebra = alg;
          |}
        """.stripMargin).classBodyDeclarations()
      case _ => Seq.empty
    }

    val str:String = s"""|package algebra;
                         |public class ${name.capitalize}$fullName${domain.baseTypeRep.name}Alg $previous implements $fullName${domain.baseTypeRep.name}Alg<${name.capitalize}> {
                         |
                         |     ${producerConstructor.mkString("\n")}
                         |     ${delegate.mkString("\n")}
                         |
                         |     ${methods.mkString("\n")}
                         |}""".stripMargin
    Java(str).compilationUnit()
  }

  /**
    * Generate interface for an operation.
    *
    * Handle Producer methods specially
    */
  def baseInterface(op:domain.Operation) : CompilationUnit = {
    val name = op.name.toLowerCase
    val tpe = typeConverter(op.returnType.get)

    val params = op match {
      case bm:domain.BinaryMethod => binaryMethodParameters(op, typeConverter)
      case _ => parameters(op)
    }

    val signature = op match {
      case b:domain.ProducerOperation => s"  $algebraAPI $name($params);"
      case _ => s"  $tpe $name($params);"
    }

    val parent:String = op match {
      case b:domain.BinaryMethod => s"extends ${domain.AsTree.name.capitalize}"
      case _ => ""
    }

    // implementations
    val str = s"""|package algebra;
                  |interface ${op.name.capitalize} $parent {
                  |  $signature
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

  /**
    * Producer operations are handled by devising a scheme to convert from Algebra into a strawman OO
    * implementation, where the data type structure is handled, and then converting back into algebra.
    *
    * This is all made possible using the Combined Algebra which is being constructed for testing purposes.
    * @return
    */
  def generateStraightOO() : Seq[CompilationUnit] = {
    val dataTypes = getModel.flatten().types.map(exp => {
      val fields = exp.attributes.flatMap(att => {
        val tpe = att.tpe match {
          case domain.baseTypeRep => Java(domain.baseTypeRep.name).tpe()
          case _ => typeConverter(att.tpe)
        }
        Java(s"private $tpe ${att.name};").fieldDeclarations()
      })

      val params:Seq[String] = exp.attributes.map(att => {
        val tpe = att.tpe match {
          case domain.baseTypeRep => Java(domain.baseTypeRep.name).tpe()
          case _ => typeConverter(att.tpe)
        }

        s"$tpe ${att.name}"
      })
      val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

      val constructor =  s"""|public ${exp.name.capitalize} (${params.mkString(",")}) {
                             |   ${cons.mkString("\n")}
                             |}""".stripMargin
      val unit = Java(s"""|package algebra.oo;
                          |public class ${exp.toString} extends ${domain.baseTypeRep.name} {
                          |  $constructor
                          |  ${fields.mkString("\n")}
                          |}""".stripMargin).compilationUnit

      // only call convert on recursive structures
      val args = exp.attributes.map(att => {
        att.tpe match {
          case domain.baseTypeRep => s"${att.name.toLowerCase}.convert(algebra)"
          case _ => s"${att.name.toLowerCase}"
        }
      }).mkString(",")
      val convertMethod = Java(s"""|public $algebraAPI convert (algebra.Combined${domain.baseTypeRep.name}Alg algebra) {
                                   |  return algebra.${exp.name.toLowerCase}($args);
                                   |}""".stripMargin).methodDeclarations()

      addMethods(unit, convertMethod)
    })

    // base
    val iface = Java(s"""|package algebra.oo;
                         |public abstract class Exp {
                         |    // Converts from OO solution back to Algebra
                         |    public abstract $algebraAPI convert (algebra.Combined${domain.baseTypeRep.name}Alg algebra);
                         |}""".stripMargin).compilationUnit()

    dataTypes :+ iface
  }
}
