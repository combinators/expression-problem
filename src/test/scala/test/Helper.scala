package test

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{MethodDeclaration, TypeDeclaration}
import org.combinators.cls.interpreter.InhabitationResult
import org.scalatest._


import scala.collection.JavaConverters._
/**
  * Defines helpers
  */
class Helper extends FunSpec {

  /**
    * Returns a List of declared methods for the only type in the given Compilation unit.
    */
  def methods(unit:CompilationUnit): List[MethodDeclaration] = {

    val clazz:TypeDeclaration[_] = unit.getTypes.get(0)
    clazz.getMethods.asScala.toList
  }

  // Can't seem to place 'it' methods within this function
  def singleClass(name: String, result: InhabitationResult[CompilationUnit]):Boolean = {
    val inhab: Iterator[CompilationUnit] = result.interpretedTerms.values.flatMap(_._2).iterator

    if (inhab.hasNext) {
      val actual = inhab.next
      val clazz = actual.getClassByName(name)
      if (!inhab.hasNext) {
        return clazz.isPresent && clazz.get().getNameAsString == name
      }
      println (name + " has more than one inhabitant.")
      false
    } else {
      println (name + " has no inhabitant.")
      false
    }
  }

  /**
    * Determine if a single instance of given type.
    */
  def singleInstance[T](result: InhabitationResult[T]):Boolean = {
    val inhab: Iterator[T] = result.interpretedTerms.values.flatMap(_._2).iterator

    if (inhab.hasNext) {
      inhab.next   // advance
      if (!inhab.hasNext) {
        return true
      }
    }

    false
  }


}
