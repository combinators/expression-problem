package example.expression.cpp   /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethod

trait CPPBinaryMethod extends BinaryMethod {
  val domain:BaseDomain with ModelDomain

  def getModel: domain.Model

  type Declaration = CPPFile

  /**
    * Declares the helper classes needed, in C++
    * @return
    */
  def declarations: Seq[CPPFile] = {
    val treeDecls:Seq[CPPElement] = s"""
         |virtual bool isLeaf() const = 0;
         |virtual bool isNode() const = 0;
         |
         |bool same (const Tree *o) const;
         |void output () const;
         |""".stripMargin.split("\n").map(s => new CPPElement(s))

    val nodeDecls:Seq[CPPElement] =
      s"""
         | bool isLeaf() const { return false; }
         | bool isNode() const { return true; }
         |   Node(std::vector<Tree*> children, int lbl) : label(lbl) {
         |    subtrees.insert(subtrees.begin(), children.begin(), children.end());
         |  }
         |  const int label;
         |  std::vector<Tree*> subtrees;
         |""".stripMargin.split("\n").map(s => new CPPElement(s))

    val headers =
      s"""
         |#include <vector>
         |#include "Tree.h"
         |""".stripMargin.split("\n")

    val leafDecls:Seq[CPPElement] =
      s"""
         |const void *value;
         |  Leaf(const void *e) {
         |    value = e;
         |  }
         |bool isNode() const { return false; }
         |bool isLeaf() const { return true; }
         |""".stripMargin.split("\n").map(s => new CPPElement(s))

    // need data type definition...
    definedDataSubTypes("", getModel.flatten().types) ++
    Seq(new CPPClass("Tree", "Tree", treeDecls, Seq.empty),
      new CPPClass("Subtypes", "Subtypes", Seq.empty, Seq.empty),
      new CPPClass("Node", "Node", nodeDecls, Seq.empty)
          .setSuperclass("Tree")
          .addHeader(headers),
      new CPPClass("Leaf", "Leaf", leafDecls, Seq.empty)
          .setSuperclass("Tree")
          .addHeader(headers),
      new StandAlone("Tree",
        s"""|#include "Tree.h"
            |#include "Node.h"
            |#include "Leaf.h"
            |#include <typeinfo>
            |#include <iostream>
            |
            |void Tree::output () const {
            |if (isLeaf()) {
            |  Leaf *leaf_this = (Leaf*)this;
            |  std::cout <<"L:[" << *((double *)leaf_this->value) << "]";;
            | } else {
            |  Node *us = (Node*) this;
            |  std::cout <<"N:[";
            |
            |  auto it_us = us->subtrees.begin();
            |
            |  Tree *obj1;
            |  while (it_us != us->subtrees.end()) {
            |    obj1 = *it_us;
            |    obj1->output();
            |    ++it_us;
            |  }
            |  std::cout <<"]";
            | }
            |}
            |
            |
          |bool Tree::same (const Tree *o) const {
          |  if (typeid(this) != typeid(o)) { return false; }
          |
          |  bool leafCheck = isLeaf() && o->isLeaf();
          |  if (leafCheck) {
          |    Leaf *leaf_this = (Leaf*)this;
          |    Leaf *leaf_o = (Leaf*)(o);
          |    return leaf_this->value == leaf_o->value;
          |  }
          |  bool nodeCheck = isNode() && o->isNode();
          |  if (!nodeCheck) { return false; }
          |
          |  Node *us = (Node*) this;
          |  Node *them = (Node*) o;
          |
          |  if (us->label != them->label) { return false; }       // must be same label
          |
          |  if (us->subtrees.size() != them->subtrees.size()) { return false; }  // short-circuit if not same length
          |  auto it_us = us->subtrees.begin();
          |  auto it_them = them->subtrees.begin();
          |
          |  Tree *obj1, *obj2;
          |    while (it_us != us->subtrees.end() && it_them != them->subtrees.end()) {
          |      obj1 = *it_us;
          |      obj2 = *it_them;
          |      if (!obj1->same(obj2)) { return false; }
          |
          |      ++it_us;
          |      ++it_them;
          |  }
          |  return true;
          |}""".stripMargin.split("\n")))
  }

  /**
    * Compute parameter "Type name" comma-separated list from operation. Be sure to convert BaseType into op.name!
    *
    * @param op               operation under consideration
    * @param typeConverter    existing typeconverter which we need for other types besides baseTypeRep
    * @return                 return new parameter type with op interface used in place of baseTypeRep
    */
  def binaryMethodParameters(op:domain.Operation, typeConverter:(domain.TypeRep,Option[CPPType]) => CPPType) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.TypeRep = tuple._2

      // use operation name for binary method
      val realType = tpe match {
        case domain.baseTypeRep => op.name.capitalize
        case _ => typeConverter(tpe, Option.empty)
      }

      realType.toString + " " + name
    }).mkString(",")
  }

  /**
    * Add defined data types for given exp subtype
    * @param context
    * @param exps
    */
  def definedDataSubTypes(context:String, exps:Seq[domain.Atomic]) :Seq[CPPFile] = {
    val realContext = if (context.equals("")) {
      ""
    } else {
      context + "."
    }

    val subtypes:Seq[String] =
      s"""
         |enum DefinedSubtypes {
         |  ${exps.map(exp => exp.name + "Subtype=" + exp.name.hashCode).mkString(",")}
         |};""".stripMargin.split("\n")

    Seq(new CPPHeaderCode("DefinedSubtypes", subtypes))
  }

  def logicAsTree(exp:domain.Atomic) : Seq[CPPElement] = {
    val args = exp.attributes.map(att => att.name).mkString(",")
          Seq(new CPPElement(
            s"""
               |public Tree ${domain.AsTree.name.toLowerCase}() {
               |  return asTree.${exp.name.toLowerCase}($args).${domain.AsTree.name.toLowerCase}();
               |}""".stripMargin))
  }

  /** Interesting shift needed for visitor. */
  // TODO: IS THIS EVEN USED?
//  def visitorLogicAsTree(exp:domain.Atomic) : Seq[CPPElement] = {
//    val atomicArgs = exp.attributes.map(att => att.name).mkString(",")
//
//    // changes whether attributes can be access *directly* or whether they are accessed via getXXX*() method.
//    val recursiveArgs = exp.attributes.map(att => "get" + att.name.capitalize + s"()->${AsTree.name.toLowerCase}()").mkString(",")
//
//    val body:Seq[CPPElement] = exp match {
//      case b:Binary => {
//
//        Seq(new CPPElement(s""" value_map_[e] =  new Node(java.util.Arrays.asList($recursiveArgs), DefinedSubtypes::${exp.name.capitalize}Subtype); """))
//      }
//      case u:Unary => {
//
//          val vec1 = s"std::vector<Tree *> vec_lit{$recursiveArgs};"
//          Seq(new CPPElement(s""" value_map_[e] =  new Node(java.util.Arrays.asList($recursiveArgs), DefinedSubtypes::${exp.name.capitalize}Subtype); """))
//      }
//      case a:Atomic => {
//        Seq(new CPPElement(s""" value_map_[e] = new Leaf($atomicArgs);"""))
//      }
//    }
//
//    Seq(new CPPElement(
//      s"""
//         |public Tree ${domain.AsTree.name.toLowerCase}() {
//         |  ${body.mkString("\n")}
//         |}""".stripMargin))
//  }
}
