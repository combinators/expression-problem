package org.combinators.ep.language.inbetween.ffi   /*DI:LI:AI*/

import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax
import org.combinators.ep.generator.paradigm.ffi.{CreateLeaf, CreateNode, Trees => Trs}

// cannot find 'trees'
trait Trees[FT <: trees.FinalTypes, FactoryType <: trees.Factory[FT]] extends Trs[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory
  val treeLibrary: Map[Seq[any.Name[FT]], Generator[any.CompilationUnit[FT], Unit]]

  override val treeCapabilities: TreeCapabilities = new TreeCapabilities {
    implicit val canCreateLeaf: Understands[any.Method[FT], Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[CreateLeaf[any.Type[FT]], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.createLeaf(command.functional.valueType, command.arguments.head))
        }
      }
    implicit val canCreateNode: Understands[any.Method[FT], Apply[CreateNode, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[CreateNode, any.Expression[FT], any.Expression[FT]]] {
        def perform(context: any.Method[FT], command: Apply[CreateNode, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.createNode(command.arguments.head, command.arguments.tail))
        }
      }
  }

  override def enable(): Generator[any.Project[FT], Unit] = {
    import base.projectCapabilities._
    import syntax.forEach
    for {
      _ <- forEach(treeLibrary.toSeq){ case (qualifiedName, compilationUnit) =>
        addCompilationUnit(compilationUnit, qualifiedName: _*)
      }
    } yield ()
  }
}

object Trees {
  type WithBase[FT <: trees.FinalTypes, FactoryType <: trees.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Trees[FT, FactoryType] { val base: B }
  def apply[FT <: trees.FinalTypes, FactoryType <: trees.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](
    _base: B)(
    _treeLibrary: Map[Seq[any.Name[FT]], Generator[any.CompilationUnit[FT], Unit]]
  ): WithBase[FT, FactoryType, _base.type] = new Trees[FT, FactoryType] {
    val base: _base.type = _base
    val treeLibrary = _treeLibrary
  }
}
