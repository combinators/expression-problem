package org.combinators.ep.language.java

import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration, Node, PackageDeclaration}
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.expr.{Name, SimpleName}
import com.github.javaparser.ast.visitor.Visitable
import org.combinators.templating.twirl.Java

class ImportCleanup {
   case class UsageAnalyzer(usageData: Map[SimpleName, Map[Option[Name], Int]] = Map.empty.withDefaultValue(Map.empty.withDefaultValue(0))) {
     def use(name: Name): UsageAnalyzer = {
       val simplePart = new SimpleName(name.getIdentifier)
       val qualifier = name.getQualifier.map[Option[Name]](Some(_)).orElse(None)
       val entry = usageData(simplePart)
       copy(usageData = usageData.updated(simplePart, entry.updated(qualifier, entry(qualifier) + 1)))
     }

     def toQualifiedName(classOrInterfaceType: ClassOrInterfaceType): Name = {
       val simpleName = classOrInterfaceType.getName.getIdentifier
       classOrInterfaceType.getScope
         .map[Option[ClassOrInterfaceType]](Some(_)).orElse(None)
         .map(toQualifiedName)
         .map(new Name(_, simpleName))
         .getOrElse(new Name(simpleName))
     }

     def toClassOrInterfaceType(qualifiedName: Name): ClassOrInterfaceType = {
       Java(qualifiedName).tpe().asClassOrInterfaceType()
     }

     def use(classOrInterfaceType: ClassOrInterfaceType): UsageAnalyzer = {
       use(toQualifiedName(classOrInterfaceType))
     }

     def mostRelevantFor(name: SimpleName): Name = {
       val dataForName = usageData(name)
       val unqualified = Option.empty[Name]
       if (dataForName.isEmpty || dataForName(unqualified) > 0) {
         new Name(name.getIdentifier)
       } else {
         dataForName.maxBy(_._2)._1
           .map(qualifier => new Name(qualifier, name.getIdentifier))
           .getOrElse(new Name(name.getIdentifier))
       }
     }

     def keepImport(importDecl: ImportDeclaration): Boolean = {
       val qualifiedImportedName = importDecl.getName
       val simpleImportedName = new SimpleName(qualifiedImportedName.getIdentifier)
       (importDecl.isStatic
         || importDecl.isAsterisk
         || (qualifiedImportedName == mostRelevantFor(simpleImportedName)
              && usageData(simpleImportedName).nonEmpty))
     }

     def simplify(name: Name): Name = {
       val simplePart = new SimpleName(name.getIdentifier)
       if (mostRelevantFor(simplePart) == name) {
         new Name(name.getIdentifier)
       } else {
         name
       }
     }


     def simplify(classOrInterfaceType: ClassOrInterfaceType): ClassOrInterfaceType = {
        toClassOrInterfaceType(simplify(toQualifiedName(classOrInterfaceType)))
     }
   }

  sealed private trait Phase
  private case object ANALYZE extends Phase
  private case object CLEANUP extends Phase

  private class CleanupVisitor extends com.github.javaparser.ast.visitor.ModifierVisitor[Phase] {
    var usageAnalyzer: UsageAnalyzer = UsageAnalyzer()

    /** Make sure to leave alone the package declaration. */
    override def visit(n: PackageDeclaration, arg: Phase): Visitable = n

    override def visit(classOrInterfaceType: ClassOrInterfaceType, phase: Phase): Visitable = {
      phase match {
        case ANALYZE =>
          usageAnalyzer = usageAnalyzer.use(classOrInterfaceType)
          classOrInterfaceType
        case CLEANUP =>
          usageAnalyzer.simplify(classOrInterfaceType)
      }
    }

    override def visit(name: Name, phase: Phase): Visitable = {
      phase match {
        case ANALYZE =>
          usageAnalyzer = usageAnalyzer.use(name)
          name
        case CLEANUP =>
          usageAnalyzer.simplify(name)
      }
    }
    override def visit(name: SimpleName, phase: Phase): Visitable = {
      phase match {
        case ANALYZE =>
          usageAnalyzer = usageAnalyzer.use(new Name(name.getIdentifier))
          name
        case _ => name
      }
    }
    override def visit(importDecl: ImportDeclaration, phase: Phase): Node = {
      phase match {
//        case CLEANUP =>
//          if (usageAnalyzer.keepImport(importDecl)) {
//            importDecl
//          } else {
//            null
//          }
        case _ => importDecl
      }
    }
  }

  def cleanup(units: CompilationUnit*): Seq[CompilationUnit] = {
    units.map(unit => {
      val cleanupVisitor = new CleanupVisitor
      unit
        .accept(cleanupVisitor, ANALYZE)
        .accept(cleanupVisitor, CLEANUP)
        .asInstanceOf[CompilationUnit]
    })
  }
}

object ImportCleanup {
  def cleaned(units: CompilationUnit*): Seq[CompilationUnit] = units
//    new ImportCleanup().cleanup(units: _*)
}
