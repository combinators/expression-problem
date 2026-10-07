package org.combinators.ep.builder.inbetween.paradigm.ffi     /*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.cogen.paradigm.{Apply, Reify, ToTargetLanguageType}
import org.combinators.ep.generator.paradigm.ffi.{CreateLeaf, CreateNode, Trees as Trs}
import org.combinators.cogen.paradigm.AnyParadigm.syntax
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.{Command, FileWithPath, TypeRep, Understands}
import org.combinators.ep.domain.abstractions.DomainTpeRep
import org.combinators.ep.language.inbetween.ffi.FFI
import org.combinators.ep.domain.tree.{Tree, Node, Leaf}

trait Trees {
  val _base: AnyParadigm { val ast: TreesAST }
  val treeLibrary: Seq[FileWithPath]

  trait TreesIn[Ctxt] extends Trs[Ctxt] with FFI[Ctxt] {
    import base.ast.any
    import base.ast.treesOpsFactory

    override val base: _base.type = _base
    val canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[_base.ast.any.Type]]
    def canReifyInCtxt[T]: Understands[Ctxt, Reify[T, _base.ast.any.Expression]]

    override val treeCapabilities : TreeCapabilities = new TreeCapabilities {
      override implicit val canCreateLeaf: Understands[Ctxt, Apply[CreateLeaf[any.Type], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[CreateLeaf[any.Type], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[CreateLeaf[any.Type], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, treesOpsFactory.createLeaf(command.functional.valueType, command.arguments.head))
          }
        }
      override implicit val canCreateNode: Understands[Ctxt, Apply[CreateNode, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[CreateNode, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[CreateNode, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, treesOpsFactory.createNode(command.arguments.head, command.arguments.tail))
          }
        }
    }

    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] = {
      case DomainTpeRep.Tree =>
        Some(Command.lift(treesOpsFactory.node()))

      case _ => None
    }

    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      case DomainTpeRep.Tree => {
        case Node(label, children) => {
          val result: Generator[Ctxt, _base.syntax.Expression] = for {
            reifiedChildren <- forEach(children) { child =>
              Reify(DomainTpeRep.Tree, child).interpret(using canReifyInCtxt)
            }

            reifiedLabel <- Reify(TypeRep.Int, label).interpret(using canReifyInCtxt)
          } yield _base.ast.treesOpsFactory.createNode(reifiedLabel, reifiedChildren.toSeq)

          Some(result)
        }

        case Leaf(element) => {
          val result: Generator[Ctxt, _base.syntax.Expression] = for {
            reifiedElement <- Reify(element.tpe, element.inst).interpret(using canReifyInCtxt)
            elementTpe <- ToTargetLanguageType(element.tpe).interpret(using canToTargetLanguage)
          } yield _base.ast.treesOpsFactory.createLeaf(elementTpe, reifiedElement)

          Some(result)
        }
      }

      case _ => _ => None
    }

    override def enable(): Generator[_base.ast.any.Project, Boolean] = {
      import _base.projectCapabilities.*
      import syntax.forEach
      
      for {
        updated <- super.enable()
        
        _ <- if (updated) {
          forEach(treeLibrary) { treeLibraryFile =>
            addCustomFile(treeLibraryFile)
          }
        } else {
          Command.skip[_base.ast.any.Project]
        }
          
      } yield updated
    }

  }
  
}

object Trees {
  type WithBase[B <: AnyParadigm, Context] = Trees { val _base: B }

  def apply[AST <: TreesAST, B <: AnyParadigm.WithAST[AST], Context](base: B)(
     _treeLibrary: Seq[FileWithPath],
    _addContextTypeLookup: (tpe: TypeRep, lookup: base.ast.any.Type) => Generator[base.ast.any.Project, Unit]
   ): WithBase[base.type, Context] = {
    class Trs(
      override val _base: base.type = base,
      override val treeLibrary: _treeLibrary.type = _treeLibrary
    ) extends Trees {
    }
    new Trs()
  }
}

