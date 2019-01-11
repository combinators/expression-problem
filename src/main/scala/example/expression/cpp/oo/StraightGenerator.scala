package example.expression.cpp.oo    /*DI:LD:AD*/

import example.expression.cpp._
import example.expression.domain.{BaseDomain, ModelDomain}

// https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions/
// straight C++ solution
trait StraightGenerator extends CPPGenerator with DataTypeSubclassGenerator with CPPBinaryMethod with StandardCPPBinaryMethod {

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

    flat.types.map(tpe => generateExp(flat, tpe)) ++
    flat.types.map(tpe => generateExpImpl(flat.ops, tpe)) :+
      defaultHeaderFile() :+
      generateBase(flat) // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:Atomic) : Map[String,CPPElement] = {
    exp.attributes.map(att => att.name -> new CPPElement(s"${att.name}")).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:CPPElement, op:Operation, params:CPPElement*) : CPPElement = {
    val args:String = params.mkString(",")
    new CPPElement(s"""e->get${expr.toString.capitalize}($args)""")
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:TypeRep) : CPPType = {
    tpe match {
      case domain.baseTypeRep => new CPPType("Exp")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): CPPType = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => new CPPType("void")
    }
  }

  /** Operations are implement ala visitor. */
  def methodGenerator(exp:Atomic)(op:Operation): CPPMethod = {
    val params = parameters(op)
    val ret = typeConverter(op.returnType.get)
    new CPPMethod(ret.toString, s"${op.name.toLowerCase}", s"(${params.mkString(",")})", logic(exp)(op).mkString("\n"))
  }

  /** Default header file needed for most classes. */
  def defaultHeaderFile() : CPPHeaderCode = {
    new CPPHeaderCode("visitor",
      s"""
         |#ifndef _VISITOR_H_
         |#define _VISITOR_H_
         |#include <iostream>
         |#include <map>
         |#include <memory>
         |#include <sstream>
         |#include <string>
         |#include <vector>
         |#endif /* _VISITOR_H_ */
       """.stripMargin.split("\n"))
  }

  // standard headers
  def standardHeader():Seq[String] = {
    s"""#include "visitor.h" """.stripMargin.split("\n")
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExpImpl(ops:Seq[Operation], sub:Atomic) : CPPFile = {

    val signatures:Seq[CPPMethod] = ops.map(op => methodGenerator(sub)(op))

    val contents =
      s"""|
         |#include "Exp.h"
         |#include "${sub.name.capitalize}.h"
         |  ${signatures.mkString("\n")}
       """.stripMargin.split("\n")

    new StandAlone(sub.name.capitalize, contents)
  }

  /** Generate the full class for the given expression sub-type (except for impl). */
  def generateExp(model:Model, sub:Atomic) : CPPFile = {
    val name = sub.name

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    var addedFields:Seq[CPPElement] = Seq.empty
    var addedMethods:Seq[CPPElement] = Seq.empty

    sub.attributes.foreach(att => {
      val capAtt = att.name.capitalize
      val tpe = typeConverter(att.tpe)

      addedFields = addedFields :+ new CPPElement(s"$tpe ${att.name}_;")

      // prepare for constructor
      params = params :+ s"const $tpe* ${att.name}"
      cons = cons :+ s"${att.name}_(${att.name})"

      // make the set/get methods
      addedMethods = addedMethods :+ new CPPElement(s"const $tpe* get$capAtt() const { return ${att.name}_; }")
    })

    // make constructor
    addedMethods = addedMethods :+
      new CPPElement (s"${sub.name} (${params.mkString(",")}) : ${cons.mkString(",")} {}")

    // add Binary methods if needed
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Seq(new CPPElement (s"""Tree *${domain.AsTree.name.toLowerCase}() const; """))
    } else {
      Seq.empty
    }
    addedMethods = addedMethods ++ astreeMethod

    new CPPClass(name, name, addedMethods, addedFields)
      .setSuperclass("Exp")
      .addHeader(standardHeader())
      .addHeader(Seq("""#include "Exp.h" """))
  }

  /** Base class interface for all Expressions */
  def generateBase(model:domain.Model):CPPFile = {

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().ops.map(op =>
        new CPPElement(s"""virtual ${typeConverter(op.returnType.get)} ${op.name.toLowerCase}() const = 0;"""))

    new CPPClass(domain.baseTypeRep.name, domain.baseTypeRep.name, allOps, Seq.empty)
  }

  def generateBinaryMethodHelpers():Seq[CPPFile] = {

    // If BinaryMethodTreeBase, need the declarations here.
    if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      declarations
    } else {
      Seq.empty
    }
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
