package org.combinators.ep.language.java

/*DI:LD:AI*/

import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, MethodDeclaration}
import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration, Node}
import com.github.javaparser.ast.expr.{CastExpr, LambdaExpr, Name, SimpleName}
import com.github.javaparser.ast.stmt.{BlockStmt, CatchClause, ForEachStmt, ForStmt, SwitchEntry, SwitchStmt}
import com.github.javaparser.ast.visitor.Visitable
import org.combinators.cogen.FreshNameProvider
import org.combinators.ep.language.java.Syntax.MangledName

class FreshNameCleanup(nameInfo: Map[String, MangledName]) {
  sealed private trait Phase

  private case object COLLECT_NAMES extends Phase

  private case object REPLACE_GENERATED extends Phase

  private class CleanupVisitor extends com.github.javaparser.ast.visitor.ModifierVisitor[Phase] {
    var freshNames: FreshNameProvider[MangledName] = FreshNameProvider[MangledName](pushName)
    private val _replaceName: scala.collection.mutable.Map[String, MangledName] = new scala.collection.mutable.HashMap[String, MangledName]()

    private final def replaceName(name: String): MangledName = {
      _replaceName.getOrElseUpdate(name, {
        val baseName = nameInfo(name)
        val (replacement, nextNames) = freshNames.freshNameBasedOn(baseName)
        freshNames = nextNames
        replacement
      })
    }

    private final def pushName(name: MangledName, useCounter: Int): MangledName = {
      if (useCounter == 0) {
        name
      } else if (useCounter == 1) {
        JavaNameProvider.addPrefix("_", name)
      } else {
        JavaNameProvider.addSuffix(name, useCounter.toString)
      }
    }

    private def visitInNewScope[N <: Visitable](n: N, phase: Phase)(doVisit: (N, Phase) => Visitable): Visitable = {
      if (phase == REPLACE_GENERATED) {
        freshNames = freshNames.pushContext
        doVisit(n, COLLECT_NAMES)
        val updated = doVisit(n, REPLACE_GENERATED)
        freshNames = freshNames.popContext
        updated
      } else n
    }

    override def visit(n: SimpleName, arg: Phase): Visitable = {
      if (arg == REPLACE_GENERATED && nameInfo.contains(n.getIdentifier)) {
        replaceName(n.getIdentifier).toAST
      } else if (arg == COLLECT_NAMES && !nameInfo.contains(n.getIdentifier)) {
        freshNames = freshNames.markUsed(MangledName.fromAST(n))
        n
      } else n
    }

    override def visit(n: Name, arg: Phase): Visitable = {
      if (arg == REPLACE_GENERATED && nameInfo.contains(n.getIdentifier)) {
        val replacement = n.clone
        replacement.setIdentifier(replaceName(n.getIdentifier).toAST.getIdentifier)
        replacement
      } else if (arg == COLLECT_NAMES && !nameInfo.contains(n.getIdentifier)) {
        freshNames = freshNames.markUsed(MangledName.fromAST(new SimpleName(n.getIdentifier)))
        n
      } else n
    }

    override def visit(n: BlockStmt, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: ClassOrInterfaceDeclaration, arg: Phase): Visitable = {
      if (arg == COLLECT_NAMES) {
        freshNames = freshNames.markUsed(MangledName.fromAST(n.getName))
      }
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: MethodDeclaration, arg: Phase): Visitable = {
      if (arg == COLLECT_NAMES) {
        freshNames = freshNames.markUsed(MangledName.fromAST(n.getName))
      }
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: ForStmt, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: ForEachStmt, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: LambdaExpr, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: ConstructorDeclaration, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: CatchClause, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: SwitchStmt, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: SwitchEntry, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }

    override def visit(n: CompilationUnit, arg: Phase): Visitable = {
      visitInNewScope(n, arg)(super.visit)
    }
  }

  def cleanup(units: CompilationUnit*): Seq[CompilationUnit] = {
    val cleanupVisitor = new CleanupVisitor
    units.map(unit => unit.accept(cleanupVisitor, REPLACE_GENERATED).asInstanceOf[CompilationUnit])
  }
}

object FreshNameCleanup {
  def cleaned(nameInfo: Map[String, MangledName], units: CompilationUnit*): Seq[CompilationUnit] =
    new FreshNameCleanup(nameInfo).cleanup(units *)
}