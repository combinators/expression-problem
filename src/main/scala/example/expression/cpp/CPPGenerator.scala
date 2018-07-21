package example.expression.cpp

import example.expression.domain.{BaseDomain, ModelDomain}
import expression.Attribute

trait CPPGenerator extends AbstractGenerator with DataTypeSubclassGenerator {

  val domain: BaseDomain with ModelDomain

  import domain._

  def getModel: domain.Model

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. A Base class to be superclass of them all
    */
  def generatedCode(): Seq[CPPFile] = {
    val flat = getModel.flatten()
    flat.types.map(tpe => generateExp(flat, tpe)) :+ // one class for each sub-type
      generateBase(flat) // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:Atomic) : Map[String,CPPElement] = {
    exp.attributes.map(att => att.name -> new CPPElement(s"${att.name}")).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:CPPElement, op:Operation, params:CPPElement*) : CPPElement = {
    val args:String = params.mkString(",")
    new CPPElement(s"""$expr.${op.name}($args)""")
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:TypeRep, covariantReplacement:Option[CPPType] = None) : CPPType = {
    tpe match {
      case domain.baseTypeRep => covariantReplacement.getOrElse(new CPPType("Exp"))
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): CPPType = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => new CPPType("void")
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:Atomic)(op:Operation): CPPMethod = {
    val params = parameters(op)
    new CPPMethod(returnType(op).toString, op.name, params, logic(exp)(op).mkString("\n"))
  }

  // standard headers
  def standardHeader():Seq[String] = {
    s"""#include "visitor.h" """.stripMargin.split("\n")
  }


  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:Model, sub:Atomic) : CPPFile = {
    val name = sub.getClass.getSimpleName

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    var addedFields:Seq[CPPElement] = Seq.empty
    var addedMethods:Seq[CPPElement] = Seq.empty

    sub.attributes.foreach(att => {
      val capAtt = att.name.capitalize
      val tpe = typeConverter(att.tpe)

      addedFields = addedFields :+ new CPPElement(s"const $tpe* ${att.name}_;")

      // prepare for constructor
      params = params :+ s"const $tpe* ${att.name}"
      cons = cons :+ s"${att.name}_(${att.name})"

      // make the set/get methods
      addedMethods = addedMethods :+ new CPPElement(s"const $tpe* get$capAtt() const { return ${att.name}_; }")
    })

    // make constructor
    addedMethods = addedMethods :+ new CPPElement (s"${sub.getClass.getSimpleName} (${params.mkString(",")}) : ${cons.mkString(",")} {}")

    // Method declaration (not implementation)
    val visitor = new CPPElement("void Accept(ExpVisitor* visitor) const;")

    addedMethods = addedMethods :+ visitor

    new CPPClass(name, name, addedMethods, addedFields)
      .setSuperclass("Exp")
      .addHeader(standardHeader())
      .addHeader(Seq("""#include "Exp.h" """, """#include "ExpVisitor.h" """))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:Model): CPPFile = {
    new CPPClass("Something", "SomethingElse", Seq.empty, Seq.empty)
//    val signatures = model.ops.flatMap(op => {
//      Java(s"public abstract ${returnType(op)} " +
//        s"${op.name}(${parameters(op)});").methodDeclarations
//    })
//
//    Java(s"""|package oo;
//             |public abstract class Exp {
//             |  ${signatures.mkString("\n")}
//             |}""".stripMargin).compilationUnit
  }

  // helper methods for C++

  /** Compute parameter "name" comma-separated list from operation. */
  def arguments(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1

      name
    }).mkString(",")
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.TypeRep = tuple._2

      typeConverter(tpe).toString + " " + name
    }).mkString(",")
  }
}
