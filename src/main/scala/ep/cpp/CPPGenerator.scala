package ep.cpp        /*DI:LD:AI*/

import java.io.File
import java.nio.file.{Path, Paths}

import ep.generator.{FileWithPath, LanguageIndependentGenerator}

/**
  * Any C++ EP approach can extend this Generator
  *

#!/bin/bash -x
g++ *.cpp  -I ../cpputest/include -L ../cpputest/cpputest_build/lib -lCppUTest -lCppUTestExt -std=c++11

  */
trait CPPGenerator extends LanguageIndependentGenerator {

  type CompilationUnit = CPPFile
  type Type = CPPType
  type Expression = CPPElement
  type Statement = CPPElement

  /**
    * Default behavior in C++ is to return an expression value.
    */
  def result (expr:Expression) : Seq[Statement] = {
    Seq(new CPPElement(s"return $expr;"))
  }

  /**
    * Return just the expression.
    */
  def valueOf(expr:Expression, params:CPPElement*): CPPElement = {
    expr
  }

  /**
    * Specially required files are placed in this area.
    *
    * Currently "build.sh"
    */
  val cppResources:String = Seq("src", "main", "resources", "cpp-resources").mkString(File.separator)


  /** Taken from scala meta web page. */
  def loadSource(entry:String*) : FileWithPath = {
    val path:Path = java.nio.file.Paths.get(cppResources, entry: _*)
    val contents = java.nio.file.Files.readAllBytes(path).map(_.toChar).mkString

    FileWithPath(contents, Paths.get(entry.head, entry.tail : _*))
  }

  /**
    *
    * Helpful snippet to get all regular files below a given directory, using
    * the specified header as the relative path to those files
    */
  def getRecursiveListOfFiles(dir: File, header:String*): Seq[FileWithPath] = {
    val these:Seq[File] = dir.listFiles
    if (these == null || these.isEmpty) {
      Seq.empty
    } else {
      val sources: Seq[FileWithPath] = these.filterNot(f => f.isDirectory).map(f => loadSource(header :+ f.getName: _*))

      sources ++ these.filter(_.isDirectory).flatMap(f => getRecursiveListOfFiles(f, header :+ f.getName: _*))
    }
  }

  /**
    * Binary methods creates helper classes in package 'tree'. Completes description
    * of tree-based structure to represent the expression, using unique values for each
    * expression sub-type.
    *
    * @return
    */
  def helperClasses():Seq[FileWithPath] = {
    getRecursiveListOfFiles(Paths.get(cppResources).toFile)
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  def dependency(op: domain.Operation): scala.List[domain.Operation] = List.empty

  /** Compute parameter "name" comma-separated list from operation. */
  def arguments(op:domain.Operation) : String = {
    op.parameters.map(param => param.name).mkString(",")
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(param => typeConverter(param.tpe).toString + " " + param.name).mkString(",")
  }
}
