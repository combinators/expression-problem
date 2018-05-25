package example.expression.cpp

import java.nio.file.{Path, Paths}

import org.combinators.templating.persistable.Persistable

/**
  * Useful constructs for synthesis. Perhaps a poor-man's AST.
  *  class $signature {
  *  public:
  *    $publicArea
  *  private:
  *    $privateArea
  * };
  *
  * Note: name is likely part of $signature, but it is being pulled out so we can name the file after it.
  */
class CPPBase {
  def indent (lines:Seq[String]):String = {
    lines.map(line => s"  $line").mkString("\n")
  }
}

/** Any CPP artifact that should be placed in a file. */
abstract class CPPFile extends CPPBase {

  /** return name of file. */
  def fileName : String
}

/** Tools for CPPFiles */
object CPPFileUtils {
  /**
    * Tell the framework to store stuff of type PythonWithPath at the location specified in Path.
    * The Path is relative to the Git repository.
    */
  implicit def PersistCPPFile: Persistable.Aux[CPPFile] = new Persistable {
    override def path(elem: CPPFile): Path = Paths.get(elem.fileName + ".cpp")
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

  override def toString:String = body.mkString("\n")

  override def fileName:String = name
}

/** Main class is a standalone class with an int main() method. */
final class MainClass (val _name:String, val _body:Seq[String]) extends CPPFile {
  val body:Seq[String] = _body
  val name:String = _name

  override def toString: String = s"""|int main() {
                                        |${indent(body)}
                                        |}""".stripMargin

  override def fileName:String = name
}

final class CPPClass (val _name:String, _signature:String, val _publicArea:Seq[String], _privateArea:Seq[String]) extends CPPFile {

  val name:String = _name
  val signature:String = _signature
  val publicArea:Seq[String] = _publicArea
  val privateArea:Seq[String] =  _privateArea

  override def fileName:String = name

  override def toString:String = {
    s"""
       |class $signature {
       |public:
       |${indent(publicArea)}
       |
       |private:
       |${indent(privateArea)}
       |};
       """.stripMargin
  }
}


/**
  * Useful constructs for synthesis. Perhaps a poor-man's AST.
  *  $signature {
  *     $body
  *  }
  */
class CPPMethod (val _signature:String, val _body:Seq[String]) extends CPPBase {

  val signature:String = _signature
  val body:Seq[String] = _body

  override def toString: String = indent(Seq(s"$signature {") ++ body ++ Seq("}"))
}

/**
  * Useful constructs for synthesis. Perhaps a poor-man's AST.
  *  $signature
  */
class CPPField (val _signature:String) extends CPPBase {

  val signature:String = _signature

  override def toString: String = indent(Seq(signature))
}
