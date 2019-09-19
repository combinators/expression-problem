package org.combinators.ep.language.java.algebra   /*DI:LD:AD*/

import com.github.javaparser.ast.body.BodyDeclaration
//import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.language.java.{JavaBinaryMethod, DomainIndependentJavaGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AlgebraGenerator extends DomainIndependentJavaGenerator { // FIX ME FIX ME with JavaBinaryMethod {
//  val domain:BaseDomain with ModelDomain
//
//  def useLambdaWherePossible: Boolean = true
//
//  /** Algebra has special decisions to make with contextDispatch in all cases. */
//  override def contextDispatch(source:Context, delta:Delta) : Expression = {
//    if (source.op.isDefined && delta.expr.isEmpty && source.op.get.isInstanceOf[domain.BinaryMethod]) {
//      // used for BinaryMethods which have already created a method to match deltaop
//      val args: String = delta.params.mkString(",")
//      Java(s"""${delta.op.get.instance}($args)""").expression()
//    } else if (delta.expr.isEmpty) {
//      val exp:domain.DataType = source.exp.get
//      recreate(exp, delta.op.get)
//    } else {
//      val expr:Expression = delta.expr.get
//      val deltaop = delta.op.get
//
//      if (source.op.isDefined && source.op.get.isInstanceOf[domain.ProducerOperation]) {
//        val op:domain.Operation = source.op.get
//        dispatch(Java(s"$expr.${op.instance}()").expression(), deltaop, delta.params : _ *)
//      } else {
//        dispatch(expr, deltaop, delta.params : _*)
//      }
//    }
//  }
//
//  /**
//    * Responsible for delegating to a new operation on the current data-type context identified
//    * by exp.
//    *
//    * Complicated in algebra since there is no concept of the current object; instead, this
//    * 'recreates' the current state, using 'null' as the children, since only the top-level
//    * is needed.
//    */
//  def recreate(exp:domain.DataType, op:domain.Operation, params:Expression*) : Expression = {
//    val m:domain.Model = getModel.lastModelWithDataTypes()
//    val fullName = m.types.sortWith(_.name < _.name).map(exp => exp.concept).mkString("")
//
//    // args can all be NULL since we are not concerned with children
//    val args = exp.attributes.map(_ => "null").mkString(",")
//    val opargs = params.mkString(",")
//
//    Java(s"new ${op.concept}$fullName${domain.baseTypeRep.concept}Alg().${exp.instance}($args).${op.instance}($opargs)").expression()
//  }
//
//  /**
//    * Generating an algebra solution requires processing the models in chronological ordering to be able
//    * to prepare the proper interfaces
//    *
//    * @return
//    */
//  override def generatedCode():Seq[CompilationUnit] = {
//    //val model = process(getModel)
//
//    //  binary methods for helper
//    val decls:Seq[CompilationUnit] = if (getModel.flatten().hasBinaryMethod) {
//      generateHelperClasses()
//    } else {
//      Seq.empty
//    }
//
//    //  producer helper classes
//    val prods:Seq[CompilationUnit] = if (getModel.flatten().ops.exists {
//      case bm: domain.ProducerOperation => true
//      case _ => false
//    }) {
//      generateStraightOO()
//    } else {
//      Seq.empty
//    }
//
//    decls ++ prods ++ processModel(getModel.inChronologicalOrder)
//  }
//
//  /**
//    * For producer operations, there is a need to instantiate objects, and one would use this
//    * method (with specific parameters) to carry this out.
//    *
//    * Note: This capability is preliminary and not yet ready for use.
//    */
//  override def inst(exp:domain.DataType, params:Expression*): CodeBlockWithResultingExpressions = {
//    CodeBlockWithResultingExpressions(
//      Java(s"algebra.${exp.instance}${params.mkString("(", ", ", ")")}").expression()
//    )
//  }
//
//  /** For straight design solution, directly access attributes by name. */
//  override def expression (exp:domain.DataType, att:domain.Attribute) : Expression = {
//    Java(s"${att.instance}").expression[Expression]()
//  }
//
//  /** Directly access local method, one per operation, with a parameter. */
//  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
//    val args:String = params.mkString(",")
//    Java(s"""$expr.${op.instance}($args)""").expression()
//  }
//
//  /** Return designated Java type associated with type, or void if all else fails. */
//  override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
//   tpe match {
//      case domain.baseTypeRep => Java("E").tpe()
//      case _ => Java ("void").tpe()  // reasonable stop
//    }
//  }
//
//  /** Generated Combined API. */
//  def algebraAPI : Type = {
//    Java(s"algebra.Combined${domain.baseTypeRep.concept}Alg.Combined").tpe()
//  }
//
//  /**
//    * Every operation gets a class whose implementation contains method implementations for
//    * all known operations. This class extends the most recently defined class for the
//    * same operation (should one exist).
//    */
//  def operationGenerator(model:domain.Model, op:domain.Operation): CompilationUnit = {
//
//    // this gets "eval" and we want the name of the Interface.
//    val returnType = typeConverter(op.returnType)
//
//    val opType = Java(op.concept).tpe()
//    var targetModel:domain.Model = null
//    var fullName:String = null
//
//    /** Computes previous ExpAlgebra class directly from the model. There are four distinct subcases. */
//    val previous:String = if (model.ops.contains(op) || model.lastModelWithOperation().isEmpty) {
//      targetModel = model.flatten()
//      if (model.types.isEmpty) {
//        fullName = model.lastModelWithDataTypes().types.sortWith(_.name < _.name).map(exp => exp.concept).mkString("")
//      } else {
//        fullName = model.types.sortWith(_.name < _.name).map(exp => exp.concept).mkString("")
//      }
//
//      ""
//    } else {
//      if (model.types.isEmpty) {
//        targetModel = model.lastModelWithDataTypes()
//      } else {
//        targetModel = model
//      }
//      fullName = targetModel.types.sortWith(_.name < _.name).map(exp => exp.concept).mkString("")
//
//      // this is different! It may be that there are NO types for the lastOperationDefined, in which case we must go
//      // back further to find one where there were types defined, and then work with that one
//      val bestModel: domain.Model = if (targetModel.equals(model)) {
//        targetModel.last.lastModelWithDataTypes()
//      } else {
//        if (model.lastModelWithOperation().types.nonEmpty) {
//          model.lastModelWithOperation()
//        } else {
//          model.lastModelWithOperation().lastModelWithDataTypes()
//        }
//      }
//
//      s"extends $opType" +
//        bestModel.types.sortWith(_.name < _.name)
//          .map(op => op.concept).mkString("") + s"${domain.baseTypeRep.concept}Alg"
//    }
//
//   // Handle binary methods...
//    val op_params = op match {
//      case bm:domain.BinaryMethod => binaryMethodParameters(op, typeConverter)
//      case _ => parameters(op)
//    }
//
//    val methods = targetModel.types.flatMap(exp => {
//
//      val subName = exp.instance   // to get proper etiquette for method names
//      val code:Seq[Statement] = logic(exp, op)
//      val signatures = code.mkString("\n")
//
//      // handle covariant typing locally
//      val params = exp.attributes.map(att =>
//        att.tpe match {
//          case domain.baseTypeRep => s"final $opType ${att.instance}"
//          case _ =>   s"final ${typeConverter(att.tpe)} ${att.instance}"
//        })
//
//      // creates method body
//      val paramList = params.mkString(",")
//
//      val helpers:Seq[BodyDeclaration[_]] = op match {
//        case bm:domain.BinaryMethod =>
//          logicAsTree(exp)
//        case _ => Seq.empty
//      }
//
//      val str = if (helpers.isEmpty && useLambdaWherePossible) {
//        Java(s"""
//                |public $opType $subName($paramList) {
//                |   return ($op_params) -> {$signatures };
//                |}""".stripMargin)
//      } else {
//        Java(s"""
//                |public $opType $subName($paramList) {
//                |  return new $opType() {
//                |    ${helpers.mkString("\n")}
//                |    public $returnType ${op.instance}($op_params) {   // was NAME
//                |        $signatures
//                |    }
//                |  };
//                |}""".stripMargin)
//      }
//
//      str.methodDeclarations
//    })
//
//    val delegate:Seq[BodyDeclaration[_]] = op match {
//      case bm:domain.BinaryMethod =>
//        Java(s"""${domain.AsTree.concept}$fullName${domain.baseTypeRep.concept}Alg asTree = new ${domain.AsTree.concept}$fullName${domain.baseTypeRep.concept}Alg();""").classBodyDeclarations()
//      case _ => Seq.empty
//    }
//
//    // if this operation is a producer, then we need special constructor
//    val producerConstructor = op match {
//      case po: domain.ProducerOperation =>
//
//        // pass in super constructor IF we extend
//        val superProducerConstructor = if (previous.isEmpty) {
//          ""
//        } else {
//          "super(alg);"
//        }
//
//        Java(s"""
//          |// Binary operations are passed in necessary algebra to work with
//          |Combined${domain.baseTypeRep.concept}Alg algebra;
//          |public $opType$fullName${domain.baseTypeRep.concept}Alg(Combined${domain.baseTypeRep.concept}Alg alg) {
//          |   $superProducerConstructor
//          |		this.algebra = alg;
//          |}
//        """.stripMargin).classBodyDeclarations()
//      case _ => Seq.empty
//    }
//
//    val str:String = s"""|package algebra;
//                         |public class $opType$fullName${domain.baseTypeRep.concept}Alg $previous implements $fullName${domain.baseTypeRep.concept}Alg<$opType> {
//                         |
//                         |     ${producerConstructor.mkString("\n")}
//                         |     ${delegate.mkString("\n")}
//                         |
//                         |     ${methods.mkString("\n")}
//                         |}""".stripMargin
//    Java(str).compilationUnit()
//  }
//
//  /**
//    * Generate interface for an operation.
//    *
//    * Handle Producer methods specially
//    */
//  def baseInterface(op:domain.Operation) : CompilationUnit = {
//    val name = op.instance
//    val tpe = typeConverter(op.returnType)
//
//    val params = op match {
//      case bm:domain.BinaryMethod => binaryMethodParameters(op, typeConverter)
//      case _ => parameters(op)
//    }
//
//    val signature = op match {
//      case b:domain.ProducerOperation => s"  $algebraAPI $name($params);"
//      case _ => s"  $tpe $name($params);"
//    }
//
//    val parent:String = op match {
//      case b:domain.BinaryMethod => s"extends ${domain.AsTree.concept}"
//      case _ => ""
//    }
//
//    // implementations
//    val str = s"""|package algebra;
//                  |interface ${op.concept} $parent {
//                  |  $signature
//                  |}""".stripMargin
//    Java(str).compilationUnit()
//  }
//
//  /**
//    * As ExpAlg is refined, must add new datatypes and be sure to extend most recently defined
//    * Interface for the algebra.
//    *
//    * Produces ever-expanding ladder of interface definitions for the *ExpAlg
//    *
//    * Only call when model.types is non-empty
//    */
//  def extendedInterface(model:domain.Model) : CompilationUnit = {
//
//    // must be based on the new dataTypes being defined in model (if none, then why here? return)
//    val types:Seq[String] = model.types.sortWith(_.name < _.name).map(exp => exp.concept)
//    val newName = types.mkString("")
//
//    val signatures = model.types.map(exp => {
//        val subName = exp.instance
//
//        val params: Seq[String] = exp.attributes
//          .map(att => s"final ${typeConverter(att.tpe)} ${att.instance}")
//
//        // creates method signature from parameters
//        val paramList = params.mkString(",")
//
//        s"""E $subName($paramList);"""
//
//      }).mkString("\n")
//
//    // when extending the first one, stop at just ExpAlg
//    val previous: String = if (model.last.lastModelWithDataTypes().isEmpty) {
//      ""
//    } else {
//      "extends " + model.last.lastModelWithDataTypes().types
//        .sortWith(_.name < _.name).map(exp => exp.concept).mkString("")
//        .concat(s"${domain.baseTypeRep.concept}Alg<E>")
//    }
//
//    Java(s"""|package algebra;
//             |interface $newName${domain.baseTypeRep.concept}Alg<E> $previous {
//             | $signatures
//             |}
//             |""".stripMargin).compilationUnit()
//  }
//
//
//  /** Starting from oldest (base) model, work forward in history. */
//  def processModel(models:Seq[domain.Model]): Seq[CompilationUnit] = {
//
//    // each one is handled individually, then by going backwards, we can find out where the base is
//    // ans work outwards from there.
//    var comps: Seq[CompilationUnit] = Seq.empty
//    var operations:Seq[domain.Operation] = Seq.empty
//
//    models.foreach(model => {
//
//      // one of these two conditions MUST be true. Either define new interface for each operation
//      // or extend existing *ExpAlg interface
//      if (model.ops.nonEmpty) {
//        model.ops.foreach(op => {
//          comps = comps :+ baseInterface (op)
//          comps = comps :+ operationGenerator(model, op)
//        })
//      }
//
//      // If new data types defined, must deal with extensions.
//      if (model.types.nonEmpty) {
//        comps = comps :+ extendedInterface(model)
//        operations.foreach(op => {
//          comps = comps :+ operationGenerator(model, op)
//        })
//      }
//
//      // maintain increasing collection of operations. As new operations are defined, one must
//      // create methods for each existing data type from the past
//      operations = operations ++ model.ops
//    })
//
//    comps
//  }
//
//  /**
//    * Producer operations are handled by devising a scheme to convert from Algebra into a strawman OO
//    * implementation, where the data type structure is handled, and then converting back into algebra.
//    *
//    * This is all made possible using the Combined Algebra which is being constructed for testing purposes.
//    * @return
//    */
//  def generateStraightOO() : Seq[CompilationUnit] = {
//    val dataTypes = getModel.flatten().types.map(exp => {
//      val fields = exp.attributes.flatMap(att => {
//        val tpe = att.tpe match {
//          case domain.baseTypeRep => Java(domain.baseTypeRep.concept).tpe()
//          case _ => typeConverter(att.tpe)
//        }
//        Java(s"private $tpe ${att.instance};").fieldDeclarations()
//      })
//
//      val params:Seq[String] = exp.attributes.map(att => {
//        val tpe = att.tpe match {
//          case domain.baseTypeRep => Java(domain.baseTypeRep.concept).tpe()
//          case _ => typeConverter(att.tpe)
//        }
//
//        s"$tpe ${att.instance}"
//      })
//     val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.instance} = ${att.instance};").statements())
//
//      val constructor =  s"""|public ${exp.concept} (${params.mkString(",")}) {
//                             |   ${cons.mkString("\n")}
//                             |}""".stripMargin
//      val unit = Java(s"""|package algebra.oo;
//                          |public class ${exp.toString} extends ${domain.baseTypeRep.concept} {
//                          |  $constructor
//                          |  ${fields.mkString("\n")}
//                          |}""".stripMargin).compilationUnit
//
//      // only call convert on recursive structures
//      val args = exp.attributes.map(att => {
//        att.tpe match {
//          case domain.baseTypeRep => s"${att.instance}.convert(algebra)"
//          case _ => s"${att.instance}"
//        }
//      }).mkString(",")
//      val convertMethod = Java(s"""|public $algebraAPI convert (algebra.Combined${domain.baseTypeRep.concept}Alg algebra) {
//                                   |  return algebra.${exp.instance}($args);
//                                   |}""".stripMargin).methodDeclarations()
//
//      addMethods(unit, convertMethod)
//    })
//
//    // base
//    val iface = Java(s"""|package algebra.oo;
//                         |public abstract class ${domain.baseTypeRep.concept} {
//                         |    // Converts from OO solution back to Algebra
//                         |    public abstract $algebraAPI convert (algebra.Combined${domain.baseTypeRep.concept}Alg algebra);
//                         |}""".stripMargin).compilationUnit()
//
//    dataTypes :+ iface
//  }
}
