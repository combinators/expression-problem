package org.combinators.ep.language.cpp.oo    /*DI:LD:AD*/

import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.language.cpp._

// straight C++ solution
trait StraightGenerator extends CPPGenerator with DataTypeSubclassGenerator with CPPBinaryMethod {

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
    flat.types.map(tpe => generateExpImpl(flat, tpe)) :+
      generateBase(flat)
  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    new CPPExpression(s"${att.instance}")
  }

  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (source.op.isEmpty) {
      // Must then use delta.expr "as is"
      val opargs = delta.params.mkString (",")
      new CPPExpression(s"(${delta.expr.get})->${delta.op.get.instance}($opargs)")
    } else if (delta.expr.isEmpty) {
      val op = delta.op.get.instance
      val args = delta.params.mkString (",")
      new CPPExpression(s"$op($args)")
    } else {
     super.contextDispatch(source, delta)
    }
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:CPPExpression, op:Operation, params:CPPExpression*) : CPPExpression = {
    val args:String = params.mkString(",")
    new CPPExpression(s"get${expr.toString.capitalize}($args)->${op.instance}()")
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:TypeRep) : CPPType = {
    tpe match {
      case domain.baseTypeRep => new CPPType("Exp *")
      case _ => super.typeConverter(tpe)
    }
  }

  // TODO: Consider removing function
  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): CPPType = typeConverter(op.returnType)

  /** Operations are implement ala visitor. */
  def methodGenerator(exp:Atomic, op:Operation): CPPMethod = {
    val params = parameters(op)
    val ret = typeConverter(op.returnType)
    new CPPMethod(ret.toString, s"${op.instance}", s"($params)", logic(exp, op).mkString("\n"))
        .setConstant()
  }

  // standard headers
  def standardHeader():Seq[String] = Seq.empty

  /** Generate the full class for the given expression sub-type. */
  def generateExpImpl(model:Model, sub:DataType) : CPPFile = {

    val signatures:Seq[CPPMethod] = model.ops.map(op => {
      val params = parameters(op)
      val ret = typeConverter(op.returnType)
      new CPPMethod(ret.toString, s"${sub.concept}::${op.instance}", s"($params)", logic(sub, op).mkString("\n"))
        .setConstant()
    })

    // all other types all need to be included to be safe
    val typeHeaders = model.flatten().types.map(tpe => s"""#include "${tpe.concept}.h" """)

    val contents =
      s"""|
         |#include "Exp.h"
         |#include "${sub.concept}.h"
         |${typeHeaders.mkString("\n")}
         |  ${signatures.mkString("\n")}
       """.stripMargin.split("\n")

    new StandAlone(sub.concept, contents)
  }

  /** Generate the full class for the given expression sub-type (except for impl). */
  def generateExp(model:Model, sub:DataType) : CPPFile = {
    val name = sub.name

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    var addedFields:Seq[CPPElement] = Seq.empty
    var addedMethods:Seq[CPPElement] = Seq.empty

    sub.attributes.foreach(att => {
      val capAtt = att.concept
      val tpe = typeConverter(att.tpe)

      addedFields = addedFields :+ new CPPStatement(s"$tpe ${att.instance};")

      // prepare for constructor
      params = params :+ s"$tpe ${att.instance}_"
      cons = cons :+ s"${att.instance}(${att.instance}_)"

      // make the set/get methods
      addedMethods = addedMethods :+ new CPPStatement(s"$tpe get$capAtt() const { return ${att.instance}; }")
    })

    // make constructor
    addedMethods = addedMethods :+
      new CPPConstructor(sub.name, s"(${params.mkString(",")}) : ${cons.mkString(",")}", Seq(new CPPStatement("")))
      //new CPPElement (s"${sub.name} (${params.mkString(",")}) : ${cons.mkString(",")} {}")

    // add Binary methods if needed
    val treeHeader = if (getModel.flatten().hasBinaryMethod) {
      Seq("""#include "DefinedSubtypes.h" """, """#include "Node.h" """, """#include "Leaf.h" """, """#include "Tree.h" """)
    } else {
      Seq.empty
    }

    // all other types all need to be included to be safe
    val typeHeaders = model.flatten().types.map(tpe => s"""#include "${tpe.concept}.h" """)

    val opMethods = model.ops.map(op => {
      val params = parameters(op)
      val ret = typeConverter(op.returnType)
      new CPPMethodDeclaration(ret.toString, s"${op.instance}", s"($params)")
        .setConstant()
    })
    addedMethods = addedMethods ++ opMethods

    val helpers = model.ops.collect { case bm:domain.BinaryMethod => bm }

    new CPPClass(name, name, addedMethods, addedFields)
      .setSuperclass("Exp")
      .addHeader(standardHeader())
      .addHeader(Seq("""#include "Exp.h" """) ++ typeHeaders ++ treeHeader)
  }

  /** Base class interface for all Expressions */
  def generateBase(model:domain.Model):CPPFile = {

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().ops.map(op => {
      val tpe:CPPType = typeConverter(op.returnType)
      val realType:String = op match {
        case po:ProducerOperation => "Exp *"
        case _ => tpe.name
      }

      val params = parameters(op)
      new CPPMethodDeclaration(s"virtual $realType", op.instance, s"($params)").setConstant().setVirtual()
      //new CPPElement(s"""virtual $realType ${op.instance}($params) const = 0;""")
    })

    // add Binary #include file if needed
    val headerIncludes = if (getModel.flatten().hasBinaryMethod) {
      Seq("""#include "Tree.h" """)
    } else {
      Seq.empty
    }

    val basic:Seq[String] = s"""
                               |#include <iostream>
                               |#include <map>
                               |#include <memory>
                               |#include <sstream>
                               |#include <string>
                               |#include <vector>
      """.split("\n")

    new CPPClass(domain.baseTypeRep.name, domain.baseTypeRep.name, allOps, Seq.empty)
        .addHeader(basic).addHeader(headerIncludes)
  }

  def generateBinaryMethodHelpers():Seq[CPPFile] = {
    if (getModel.flatten().hasBinaryMethod) {
      declarations
    } else {
      Seq.empty
    }
  }
}
