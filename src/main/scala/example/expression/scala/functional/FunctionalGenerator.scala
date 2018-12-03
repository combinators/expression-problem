package example.expression.scala.functional   /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.scala._

import scala.meta.{Stat, Term}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait FunctionalGenerator extends ScalaGenerator with ScalaBinaryMethod with StandardScalaBinaryMethod {

  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel:domain.Model

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. A Base class to be superclass of them all
    */
  def generatedCode():Seq[ScalaWithPath] = {
    val flat = getModel.flatten()

    //  binary methods for helper
    val decls:Seq[ScalaWithPath] = if (flat.ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ getModel.inChronologicalOrder.tail.map(m => generateExp(m)) :+    // one trait for each extensions (skip base)
      generateBase(getModel.base())                                            // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Scala(s"${att.name}").expression()).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    var opParams = ""
    val args:String = if (params.isEmpty) {
      ""
    } else {
      // hack for now: first k params are for operation (if it has any arguments)
      // then remaining are for parameters
      if (op.parameters.nonEmpty) {
        opParams = params.take(op.parameters.length).mkString(",")
        val rest = params.takeRight(params.length - op.parameters.length)
        if (rest.isEmpty) {
          ""
        } else {
          "(" + rest.mkString(",") + ")"
        }
      } else {
        "(" + params.mkString(",") + ")"
      }
    }  // new ${op.name.capitalize}($opParams).
    Scala(s"apply($expr)$args").expression()
  }

  /**
    * Responsible for delegating to a new operation on the current context.
    */
  override def delegate(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val opargs = params.mkString(",")
    val term = Term.Name(op.name.toLowerCase)   // should be able to be ..$params
    Scala(s"new ${op.name.capitalize}().apply(new ${exp.name.capitalize}($opargs))").expression()
  }

  /**
    * Responsible for dispatching sub-expressions with possible parameter(s).
    */
  override def dependentDispatch(expr:Term, op:domain.Operation, params:Term*) : Term = {
    var opParams = ""
    val args:String = if (params.isEmpty) {
      ""
    } else {
      // hack for now: first k params are for operation (if it has any arguments)
      // then remaining are for parameters
      if (op.parameters.nonEmpty) {
        opParams = params.take(op.parameters.length).mkString(",")
        val rest = params.takeRight(params.length - op.parameters.length)
        if (rest.isEmpty) {
          ""
        } else {
          "(" + rest.mkString(",") + ")"
        }
      } else {
        "(" + params.mkString(",") + ")"
      }
    }
    Scala(s"new ${op.name.capitalize}($opParams).apply($expr)$args").expression()
  } // Scala(s"apply($expr)$args").expression()

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Scala("Unit").tpe()
    }
  }

  /**
    * Operations are implemented as methods in the Base and sub-type classes.
    * Encapsulate with braces in case multiple statements
    */
  def methodGenerator(exp:Atomic)(op:Operation): Stat = {
    val str = s"""
           |def visit${exp.name.capitalize}(${standardArgs(exp)}) : Unit = {
           |  result = {
           |    ${logic(exp)(op).mkString("\n")}
           |  }
           |}""".stripMargin
    println("mg:" + str)
    Scala(str).statement()
  }

  /**
    * Generate the full class for the given expression sub-type.
    *
    * If m.ops is empty, then need to add new data types for all past operations
    * else If m.types is empty, then need to define new operations for all past data types
    * else BOTH are present (both new ops and new types) so
    *    1. new operations for all past datatypes *and new datatypes*
    *    2. new data types for all past operations
    * Only need to bring in those datatypes which have been defined anew.
    */
  def generateExp(m:Model) : ScalaWithPath = {
    val mcaps = m.name.capitalize
    val prior = m.last.name.capitalize

    // visitor for each extension must extend prior one
    val visitors = m.types.map(exp => {
      Scala(s"def visit${exp.name.capitalize}(${standardArgs(exp)}) : Unit").statement()
    })

    // All newly defined types get their own class with visit method
    val classes = m.types.map(exp => {
      Scala(s"""
               |class ${exp.name.capitalize}(${standardArgs(exp)}) extends ${domain.baseTypeRep.name} {
               |  def accept(v: visitor): Unit = v.visit${exp.name.capitalize}(${standardParams(exp)})
               |}""".stripMargin).declaration()
    })

    // any newly defined data types must be integrated to work with past operations, including
    // any operations defined for this model. If no types defined, them ust have operations,
    // otherwise a null model.
    var set = m.ops
    if (m.types.nonEmpty) { set = m.pastOperations() }
    val ops = set.map(op => {
      var result = ""
      val set = if (m.types.isEmpty) {
        result =
          s"""
             |  var result: ${typeConverter(op.returnType.get)} = _
             |  def apply(t: ${domain.baseTypeRep.name}): ${typeConverter(op.returnType.get)} = {
             |    t.accept(this)
             |    result
             |  }
         """.stripMargin

        m.pastDataTypes()
      } else {
        m.types
      }

      // Each refined operation gets a refined class. Newly defined operations get their
      // own new top-class
      var extendsClause:String = ""
      var typesToGenerate:Seq[Atomic] = Seq.empty
      if (m.ops.contains(op)) {
        // newly defined. All past dataTypes plus current ones
        extendsClause = ""
        typesToGenerate = m.pastDataTypes()
      } else {
        // refining earlier operation. Only need to add new types
        extendsClause = s"super.${op.name.capitalize} with"
        typesToGenerate = m.types
      }

      // binary methods pass in parameters
      val binary:String = op match  {
        case _:domain.BinaryMethod => {
          s"(val ${base.that}:${domain.baseTypeRep.name})"
        }

        case _ => ""
      }
      // Data types that had existed earlier
      val baseMembers = typesToGenerate.map(exp => methodGenerator(exp)(op))
      Scala(s"""
               |class ${op.name.capitalize}$binary extends $extendsClause Visitor { self: visitor =>
               |  $result
               |  ${baseMembers.mkString("\n")}
               |}""".stripMargin).declaration()
    })

    val str =
      s"""
        |package scala_func
        |trait $mcaps extends $prior {
        |  type visitor <: Visitor
        |  trait Visitor extends super.Visitor { self: visitor =>
        |    ${visitors.mkString("\n")}
        |  }
        |  ${classes.mkString("\n")}
        |
        |  ${ops.mkString("\n")}
        |}""".stripMargin

    println(str)
    ScalaWithPath(
      Scala(str).source(), Paths.get(s"$mcaps.scala"))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(m:Model): CompilationUnit = {

    val ops = m.ops.map(op => {
      val baseMembers = m.types.map(exp => methodGenerator(exp)(op))
      Scala(s"""
           |class ${op.name.capitalize} extends Visitor { self: visitor =>
           |  var result: ${typeConverter(op.returnType.get)} = _
           |  def apply(t: ${domain.baseTypeRep.name}): ${typeConverter(op.returnType.get)} = {
           |    t.accept(this)
           |    result
           |  }
           |  ${baseMembers.mkString("\n")}
           |}""".stripMargin).declaration()
    })

    val classes = m.types.map(exp => {
      Scala(s"""
         |class ${exp.name.capitalize}(${standardArgs(exp)}) extends ${domain.baseTypeRep.name} {
         |  def accept(v: visitor): Unit = v.visit${exp.name.capitalize}(${standardParams(exp)})
         |}""".stripMargin).declaration()
    })

    val visitors = m.types.map(exp => {
      Scala(s"def visit${exp.name.capitalize}(${standardArgs(exp)}) : Unit").statement()
    })

    val mcaps = m.name.capitalize
    val str:String = s"""
                  |package scala_func
                  |trait $mcaps {
                  |  trait ${domain.baseTypeRep.name} {
                  |    def accept(v: visitor): Unit
                  |  }
                  |
                  |  ${classes.mkString("\n")}
                  |  type visitor <: Visitor
                  |  trait Visitor {
                  |     ${visitors.mkString("\n")}
                  |  }
                  |
                  |  // base operations
                  |  ${ops.mkString("\n")}
                  |}""".stripMargin

    println ("S:" + str)
    ScalaWithPath(
      Scala(str).source(), Paths.get(s"$mcaps.scala"))
  }
}
