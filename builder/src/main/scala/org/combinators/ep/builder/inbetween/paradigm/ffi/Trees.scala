package org.combinators.ep.builder.inbetween.paradigm.ffi

/*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}
import org.combinators.ep.language.inbetween.{any, polymorphism}
import org.combinators.cogen.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{CreateLeaf, CreateNode, Trees as Trs}
import org.combinators.cogen.paradigm.AnyParadigm.syntax
import org.combinators.cogen.{Command, TypeRep, Understands}
import org.combinators.ep.domain.abstractions.DomainTpeRep
import org.combinators.ep.language.inbetween.ffi.OperatorExpressionOps

trait Trees[Context](val base: AnyParadigm2.WithAST[TreesAST]) extends Trs[Context] {
  import base.ast.treesOpsFactory
  import base.ast.any
  val treeLibrary: Map[Seq[any.Name], Generator[any.CompilationUnit, Unit]]
  def addContextTypeLookup(tpe: TypeRep, lookup: any.Type): Generator[any.Project, Unit]

  override val treeCapabilities: TreeCapabilities = new TreeCapabilities {
    implicit val canCreateLeaf: Understands[Context, Apply[CreateLeaf[any.Type], any.Expression, any.Expression]] =
      new Understands[Context, Apply[CreateLeaf[any.Type], any.Expression, any.Expression]] {
        def perform(context: Context, command: Apply[CreateLeaf[any.Type], any.Expression, any.Expression]): (Context, any.Expression) = {
          (context, treesOpsFactory.createLeaf(command.functional.valueType, command.arguments.head))
        }
      }
    implicit val canCreateNode: Understands[Context, Apply[CreateNode, any.Expression, any.Expression]] =
      new Understands[Context, Apply[CreateNode, any.Expression, any.Expression]] {
        def perform(context: Context, command: Apply[CreateNode, any.Expression, any.Expression]): (Context, any.Expression) = {
          (context, treesOpsFactory.createNode(command.arguments.head, command.arguments.tail))
        }
      }
  }

  override def enable(): Generator[any.Project, Unit] = {
    import base.projectCapabilities.*
    import syntax.forEach
    for {
      _ <- forEach(treeLibrary.toSeq){ case (qualifiedName, compilationUnit) =>
        addCompilationUnit(compilationUnit, qualifiedName*)
      }
      nodeTpe <- Command.lift(treesOpsFactory.node())
      _ <- addContextTypeLookup(DomainTpeRep.Tree, nodeTpe)
    } yield ()
  }
}

object Trees {

  type WithBase[AST <: TreesAST, B <: AnyParadigm2.WithAST[AST], Context] = Trees[Context] {val base: B}
  trait WB[AST <: TreesAST, B <: AnyParadigm2.WithAST[AST], Context](override val base: B) extends Trees[Context] {}

  def apply[AST <: TreesAST, B <: AnyParadigm2.WithAST[AST], Context](
     _base: B)(
     _treeLibrary: Map[Seq[_base.ast.any.Name], Generator[_base.ast.any.CompilationUnit, Unit]],
    _addContextTypeLookup: (tpe: TypeRep, lookup: _base.ast.any.Type) => Generator[_base.ast.any.Project, Unit]
   ): WithBase[AST, _base.type, Context] = new WB[AST, _base.type, Context](_base) with Trees[Context](_base) {
    override val treeLibrary: _treeLibrary.type = _treeLibrary
    override def addContextTypeLookup(tpe: TypeRep, lookup: base.ast.any.Type): Generator[base.ast.any.Project, Unit] = _addContextTypeLookup(tpe, lookup)
  }
}

