package org.combinators.ep.language.cpp    /*DI:LD:AI*/

import java.nio.file.{Path, Paths}
import org.combinators.templating.persistable.Persistable

/**
  * Base class of all C++ artifacts
  */
class CPPBase {
  def indent (lines:Seq[Any]):String = {
    lines.map(line => s"  $line").mkString("\n")
  }
}

/**
  * A C++ Field or Method
  */
abstract class CPPElement (val stmt:String = "") extends CPPBase {
  override def toString:String = stmt
}

/** An Expression in C++. */
class CPPExpression(val exp:String = "") extends CPPElement(exp)

/** A Statement that either ends in a ; or is a fully-formed block/ */
class CPPStatement(val s:String = "") extends CPPElement(s)


/**
  * A valid identifier of a type.
  */
class CPPType (val stmt:String = "") extends CPPBase {
  override def toString:String = stmt
}

/** Any CPP artifact that should be placed in a file. */
abstract class CPPFile extends CPPBase {
  var standardHeader:String = ""

  def isHeader:Boolean = false

  // allow one to extend include definitions
  def addHeader(s:Seq[String]): CPPFile = {
    standardHeader = standardHeader + "\n" + s.mkString("\n")
    this
  }

  /** return name of file. */
  def fileName : String
}

/** Tools for CPPFiles */
object CPPFileUtils {
  /**
    * Tell the framework to store stuff of type CPPFile at the location specified in Path.
    * The Path is relative to the Git repository.
    */
  implicit def PersistCPPFile: Persistable.Aux[CPPFile] = new Persistable {
    override def path(elem: CPPFile): Path = {
      if (elem.isHeader) {
        Paths.get(elem.fileName + ".h")
      } else {
        Paths.get(elem.fileName + ".cpp")
      }
    }

    override def rawText(elem: CPPFile): Array[Byte] = elem.toString.getBytes
    override type T = CPPFile
  }
}

/**
  * Useful for header files, or forward reference to class def ahead of real class def
  */
final class StandAlone(val _name:String, val _body:Seq[String]) extends CPPFile {
  val body:Seq[String] = _body
  val name:String = _name

  override def toString: String = s"""|$standardHeader
                                      |${indent(body)}
                                      |""".stripMargin


  override def fileName:String = name
}

/** Main class is a standalone class with an int main() method. */
final class MainFile (val _name:String, val _body:Seq[String]) extends CPPFile {
  val body:Seq[String] = _body
  val name:String = _name

  override def toString: String = s"""|$standardHeader
                                      |int main() {
                                      |${indent(body)}
                                      |}""".stripMargin

  // allow one to extend include definitions
  override def addHeader(s:Seq[String]): MainFile = {
    standardHeader = standardHeader + "\n" + s.mkString("\n")
    this
  }

  override def fileName:String = name
}

final class CPPHeaderCode (val _name:String, val _body:Seq[String]) extends CPPFile {
  val name:String = _name
  val body:Seq[String] = _body

  override def isHeader:Boolean = true
  override def fileName:String = name

  // allow one to extend include definitions
  override def addHeader(s:Seq[String]): CPPHeaderCode = {
    standardHeader = standardHeader + "\n" + s.mkString("\n")
    this
  }

  override def toString:String = {

    s"""
       |$standardHeader
       |${body.mkString("\n")}
       """.stripMargin
  }
}

// Code that contains an implementation
final class CPPCode (val _name:String, val _body:Seq[CPPElement]) extends CPPFile {
  val name:String = _name
  val body:Seq[CPPElement] = _body

  //override def isHeader:Boolean = false
  override def fileName:String = name

  // allow one to extend include definitions
  override def addHeader(s:Seq[String]): CPPCode = {
    standardHeader = standardHeader + "\n" + s.mkString("\n")
    this
  }

  override def toString:String = {
    val code:Seq[String] = body.map(elt => elt.toString).toSeq

    s"""|$standardHeader
        |${code.mkString("\n")}
        """.stripMargin
  }
}

/**
  * Represents a C++ class, which has a public and private area, in addition to include files.
  *  class signature {
  *  public:
  *    publicArea
  *  private:
  *    privateArea
  * };
  */
final class CPPClass (val _name:String, _signature:String, val _publicArea:Seq[CPPElement], _privateArea:Seq[CPPElement]) extends CPPFile {

  val name:String = _name
  val signature:String = _signature
  val publicArea:Seq[CPPElement] = _publicArea
  val privateArea:Seq[CPPElement] =  _privateArea
  var superClass : String = ""

  override def fileName:String = name
  override def isHeader():Boolean = true

  // allow one to extend include definitions
  override def addHeader(s:Seq[String]): CPPClass = {
    standardHeader = standardHeader + "\n" + s.mkString("\n")
    this
  }

  def setSuperclass(sup:String): CPPClass = {
    superClass = s": public $sup"
    this
  }

  override def toString:String = {
    s"""
       |#ifndef _${name}_
       |#define _${name}_
       |$standardHeader
       |
       |class $signature $superClass {
       |public:
       |${indent(publicArea)}
       |
       |private:
       |${indent(privateArea)}
       |};
       |#endif /* _${name}_ */
       """.stripMargin
  }
}

/**
  * Represents the declaration of a C++ method, suitable to be included within a header .h file.
  */
class CPPMethodDeclaration (val _retType:String, val _name:String, val _params:String) extends CPPElement {

  val retType:String = _retType
  val name:String = _name
  val params:String = _params
  var const:String = ""
  var virtual:String = ""

  def setConstant(): CPPMethodDeclaration = {
    const = " const"
    this
  }

  def setVirtual(): CPPMethodDeclaration = {
    virtual = "= 0"
    this
  }

  override def toString: String = {
    val signature = s"$retType $name$params$const$virtual"
    indent(Seq(s"$signature;"))
  }
}

/**
  * Represents the implementation of a C++ method, suitable to be included within a header .h file.
  *
  */
class CPPMethod (val _retType:String, val _name:String, val _params:String, val _body:Seq[String]) extends CPPElement {

  /**
    * Just a single statement
    */
  def this (_retType:String, _name:String, _params:String, _body:String) {
    this(_retType, _name, _params, Seq(_body))
  }

  val retType:String = _retType
  val name:String = _name
  val params:String = _params
  val body:Seq[String] = _body
  var const:String = ""

  def setConstant(): CPPMethod = {
    const = " const"
    this
  }

  override def toString: String = {
    val signature = s"$retType $name$params$const"
    indent(Seq(s"$signature {") ++ body ++ Seq("}"))
  }
}

/**
  * Represents the implementation of a C++ method, suitable to be included within a header .h file.
  *
  */
class CPPConstructor (val _name:String, val _params:String, val _body:Seq[CPPStatement]) extends CPPElement {

//  /**
//    * Just a single statement
//    */
//  def this (_name:String, _params:String, _body:String) {
//    this(_name, _params, Seq(_body))
//  }

  val name:String = _name
  val params:String = _params
  val body = _body.mkString("\n")

  override def toString: String = {
    val signature = s"$name $params "
    indent(Seq(s"$signature {") ++ body ++ Seq("}"))
  }
}

/**
  * Represents a field for a C++ class
  */
class CPPField (val _signature:String) extends CPPElement {

  val signature:String = _signature

  override def toString: String = indent(Seq(signature))
}
