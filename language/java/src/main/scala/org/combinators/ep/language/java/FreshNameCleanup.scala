package org.combinators.ep.language.java

import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, MethodDeclaration}
import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration, Node}
import com.github.javaparser.ast.expr.{CastExpr, LambdaExpr, Name, SimpleName}
import com.github.javaparser.ast.stmt.{BlockStmt, CatchClause, ForEachStmt, ForStmt, SwitchEntry, SwitchStmt}
import com.github.javaparser.ast.visitor.Visitable
import org.combinators.ep.generator.FreshNameProvider
import org.combinators.ep.language.java.Syntax.MangledName
import org.combinators.templating.twirl.Java

class FreshNameCleanup(nameInfo: Map[String, MangledName]) {
  sealed private trait Phase
  private case object COLLECT_NAMES extends Phase
  private case object REPLACE_GENERATED extends Phase

  private class CleanupVisitor extends com.github.javaparser.ast.visitor.ModifierVisitor[Phase] {
    var freshNames: FreshNameProvider[MangledName] = FreshNameProvider[MangledName](pushName)
    val replaceName: scala.collection.mutable.Map[String, MangledName] =
      new scala.collection.mutable.HashMap[String, MangledName]() {
        override def apply(name: String): MangledName = {
          getOrElseUpdate(name, {
            val baseName = nameInfo(name)
            val (replacement, nextNames) = freshNames.freshNameBasedOn(baseName)
            freshNames = nextNames
            replacement
          })
        }
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

    private def visitInNewContext[N](n: N)(doVisit: (N, Phase) => Visitable): Visitable = {
      freshNames = freshNames.pushContext
      doVisit(n, COLLECT_NAMES)
      val updated = doVisit(n, REPLACE_GENERATED)
      freshNames = freshNames.popContext
      updated
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
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: ClassOrInterfaceDeclaration, arg: Phase): Visitable = {
      freshNames = freshNames.markUsed(MangledName.fromAST(n.getName))
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: MethodDeclaration, arg: Phase): Visitable = {
      freshNames = freshNames.markUsed(MangledName.fromAST(n.getName))
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: ForStmt, arg: Phase): Visitable = {
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: ForEachStmt, arg: Phase): Visitable = {
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: LambdaExpr, arg: Phase): Visitable = {
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: ConstructorDeclaration, arg: Phase): Visitable = {
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: CatchClause, arg: Phase): Visitable = {
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: SwitchStmt, arg: Phase): Visitable = {
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: SwitchEntry, arg: Phase): Visitable = {
      visitInNewContext(n)(super.visit)
    }

    override def visit(n: CompilationUnit, arg: Phase): Visitable = {
      visitInNewContext(n)(super.visit)
    }
  }

  def cleanup(units: CompilationUnit*): Seq[CompilationUnit] = {
    val cleanupVisitor = new CleanupVisitor
    units.map(unit => unit.accept(cleanupVisitor, COLLECT_NAMES).asInstanceOf[CompilationUnit])
  }
}

object FreshNameCleanup {
  def cleaned(nameInfo: Map[String, MangledName], units: CompilationUnit*): Seq[CompilationUnit] =
    new FreshNameCleanup(nameInfo).cleanup(units: _*)
}