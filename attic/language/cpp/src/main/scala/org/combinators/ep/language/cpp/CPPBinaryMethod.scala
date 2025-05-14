package org.combinators.ep.language.cpp   /*DI:LD:AI*/

import org.combinators.ep.domain.BaseDomain

trait CPPBinaryMethod {
  val domain:BaseDomain with ModelDomain

  def getModel: domain.Model

  type Declaration = CPPFile
  type expt = CPPElement

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
         |""".stripMargin.split("\n").map(s => new CPPStatement(s))

    val nodeDecls:Seq[CPPElement] =
      s"""
         | bool isLeaf() const { return false; }
         | bool isNode() const { return true; }
         |   Node(std::vector<Tree*> children, int lbl) : label(lbl) {
         |    subtrees.insert(subtrees.begin(), children.begin(), children.end());
         |  }
         |  const int label;
         |  std::vector<Tree*> subtrees;
         |""".stripMargin.split("\n").map(s => new CPPStatement(s))

    val headers =
      s"""
         |#include <vector>
         |#include "Tree.h"
         |""".stripMargin.split("\n")

    val leafDecls:Seq[CPPElement] =
      s"""
         |Leaf(double v) {
         |    value = v;
         |}
         |double getValue() const { return value; }
         |bool isNode() const { return false; }
         |bool isLeaf() const { return true; }
         |""".stripMargin.split("\n").map(s => new CPPStatement(s))

    // need data type definition...
    definedDataSubTypes("", getModel.flatten().types) ++
    Seq(new CPPClass("Tree", "Tree", treeDecls, Seq.empty),
      new CPPClass("Subtypes", "Subtypes", Seq.empty, Seq.empty),
      new CPPClass("Node", "Node", nodeDecls, Seq.empty)
          .setSuperclass("Tree")
          .addHeader(headers),
      new CPPClass("Leaf", "Leaf", leafDecls, Seq(new CPPStatement("double value;")))
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
            |  std::cout <<"L:[" << leaf_this->getValue() << "]";;
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
          |    return leaf_this->getValue() == leaf_o->getValue();
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
    * Add defined data types for given exp subtype
    * @param context
    * @param exps
    */
  def definedDataSubTypes(context:String, exps:Seq[domain.DataType]) :Seq[CPPFile] = {
    val realContext = if (context.equals("")) {
      ""
    } else {
      context + "."
    }

    val subtypes:Seq[String] =
      s"""
         |#ifndef _DEFINEDSUBTYPES_
         |#define _DEFINEDSUBTYPES_
         |enum DefinedSubtypes {
         |  ${exps.map(exp => exp.name + "Subtype=" + exp.name.hashCode).mkString(",")}
         |};
         |#endif    /* _DEFINEDSUBTYPES_ */
         |""".stripMargin.split("\n")

    Seq(new CPPHeaderCode("DefinedSubtypes", subtypes))
  }

  def logicAsTree(exp:domain.DataType) : Seq[CPPElement] = {
    val args = exp.attributes.map(att => att.instance).mkString(",")
          Seq(new CPPMethod("Tree *", domain.AsTree.instance, "()", Seq(s"return asTree.${exp.instance}($args).${domain.AsTree.instance}();")))
//            s"""
//               |Tree *${domain.AsTree.instance}() {
//               |  return asTree.${exp.instance}($args).${domain.AsTree.instance}();
//               |}""".stripMargin))
  }

}
