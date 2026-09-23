package org.combinators.ep.builder.inbetween.paradigm.ffi

/*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.{any, polymorphism}
import org.combinators.cogen.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{CreateLeaf, CreateNode, Trees as Trs}
import org.combinators.cogen.paradigm.AnyParadigm.syntax
import org.combinators.cogen.{Command, FileWithPath, TypeRep, Understands}
import org.combinators.ep.domain.abstractions.DomainTpeRep

trait Trees[Context] extends Trs[Context] {
  override val base: AnyParadigm { val ast: TreesAST }
  import base.ast.treesOpsFactory
  import base.ast.any
  val treeLibrary: Seq[FileWithPath]
  def addContextTypeLookup(tpe: TypeRep, lookup: any.Type): Generator[any.Project, Unit]

  trait InbetweenTreeCapabilities extends super.TreeCapabilities {
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

  override val treeCapabilities: InbetweenTreeCapabilities

  override def enable(): Generator[any.Project, Unit] = {
    import base.projectCapabilities.*
    import syntax.forEach
    for {
      _ <- forEach(treeLibrary){  treeLibraryFile =>
        addCustomFile(treeLibraryFile)
      }
      nodeTpe <- Command.lift(treesOpsFactory.node())
      _ <- addContextTypeLookup(DomainTpeRep.Tree, nodeTpe)
    } yield ()
  }
}

object Trees {

  type WithBase[B <: AnyParadigm, Context] = Trees[Context] { val base: B }

  def apply[AST <: TreesAST, B <: AnyParadigm.WithAST[AST], Context](
     _base: B)(
     _treeLibrary: Seq[FileWithPath],
    _addContextTypeLookup: (tpe: TypeRep, lookup: _base.ast.any.Type) => Generator[_base.ast.any.Project, Unit]
   ): WithBase[_base.type, Context] = {
    class Trs(
      override val base: _base.type = _base,
      override val treeLibrary: _treeLibrary.type = _treeLibrary,
    ) extends Trees[Context]  {
      override val treeCapabilities: InbetweenTreeCapabilities = new InbetweenTreeCapabilities {}
      override def addContextTypeLookup(tpe: TypeRep, lookup: base.ast.any.Type): Generator[base.ast.any.Project, Unit] = _addContextTypeLookup(tpe, lookup)
    }
    new Trs()
  }
}

