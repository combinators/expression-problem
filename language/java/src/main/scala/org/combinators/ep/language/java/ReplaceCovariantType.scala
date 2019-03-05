package org.combinators.ep.language.java   /*DI:LD:AI*/

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{ConstructorDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.visitor.VoidVisitorAdapter

trait ReplaceCovariantType {
  /** Inplace modification to replace a type in every covariant position. **/
  def replaceInCovariantPosition(original: Type, by: Type): Unit
}

/** Typeclass instances **/
object ReplaceCovariantType {

  private case class CompilationUnitReplacementVisitor(original: Type, by: Type)
    extends VoidVisitorAdapter[Unit] {

    override def visit(field: FieldDeclaration, arg: Unit): Unit = {
      val elemTy = field.getElementType
      if (elemTy == original) elemTy.replace(by)
    }
    override def visit(constructorDeclaration: ConstructorDeclaration, arg: Unit): Unit = {
      constructorDeclaration.getParameters.forEach( param => {
        val paramTy = param.getType
        if (paramTy == original) paramTy.replace(by)
      })
    }
    override def visit(methodDeclaration: MethodDeclaration, arg: Unit): Unit = {
      val resultTy = methodDeclaration.getType
      if (resultTy == original) resultTy.replace(by)

      methodDeclaration.getParameters.forEach( param => {
        val paramTy = param.getType
        if (paramTy == original) paramTy.replace(by)
      })
    }
  }

  /** Covariant type replacement in CompilationUnits **/
  implicit def compilationUnitReplacement(cu: CompilationUnit): ReplaceCovariantType = new ReplaceCovariantType {
    def replaceInCovariantPosition(original: Type, by: Type): Unit = {
      cu.accept(CompilationUnitReplacementVisitor(original, by), ())
    }
  }

  /** Covariant type replacement in Methods **/
  implicit def methodReplacement(md: MethodDeclaration): ReplaceCovariantType = new ReplaceCovariantType {
    def replaceInCovariantPosition(original: Type, by: Type): Unit = {
      md.getParameters.forEach( param => {
        val paramTy = param.getType
        if (paramTy == original) paramTy.replace(by)
      })
    }
  }

}

