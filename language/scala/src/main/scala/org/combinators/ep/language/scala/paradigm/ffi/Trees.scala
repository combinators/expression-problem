package org.combinators.ep.language.scala.paradigm.ffi    /*DI:LD:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.tree.{Leaf, Node}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.generator.paradigm.ffi.{Lists => Lsts, _}
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.language.scala.CodeGenerator.Enable
import org.combinators.ep.language.scala.{ContextSpecificResolver, OperatorExprs, ProjectCtxt, ScalaNameProvider}
import org.combinators.ep.language.scala.paradigm.{AnyParadigm, Functional}

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala3._

class Trees[Ctxt, AP <: AnyParadigm](val base: AP) extends org.combinators.ep.generator.paradigm.ffi.Trees[Ctxt] {
  case object TreesEnabled
  import base.syntax._

  val pkgName = Seq("org", "combinators", "ep", "util").map(ScalaNameProvider.mangle)
  val pkg = AnyParadigm.toTermSelection(pkgName)

  val treeTerm: Term = Term.Select(pkg, Term.Name("Tree"))
  val treeType: scala.meta.Type = Type.Select(pkg, Type.Name("Tree"))

  def leafCreation[Ctxt]: Understands[Ctxt, Apply[CreateLeaf[Type], Expression, Expression]] =
    new Understands[Ctxt, Apply[CreateLeaf[Type], Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[CreateLeaf[Type], Expression, Expression]
      ): (Ctxt, Expression) = {
        val leafCtor = Term.Select(treeTerm, Term.Name("Leaf"))
        (context, Term.Apply(Term.ApplyType(leafCtor, List(command.functional.valueType)), command.arguments.toList))
      }
    }

  def nodeCreation[Ctxt]: Understands[Ctxt, Apply[CreateNode, Expression, Expression]] =
    new Understands[Ctxt, Apply[CreateNode, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[CreateNode, Expression, Expression]
      ): (Ctxt, Expression) = {
        val nodeCtor = Term.Select(treeTerm, Term.Name("Node"))
        (context, Term.Apply(nodeCtor, command.arguments.toList))
      }
    }

  override val treeCapabilities: TreeCapabilities =
    new TreeCapabilities {
      implicit val canCreateLeaf: Understands[Ctxt, Apply[CreateLeaf[Type], Expression, Expression]] =
        leafCreation
      implicit val canCreateNode: Understands[Ctxt, Apply[CreateNode, Expression, Expression]] =
        nodeCreation
    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        if (!context.resolver.resolverInfo.contains(TreesEnabled)) {

          def updateResolver(resolver: ContextSpecificResolver): ContextSpecificResolver = {
            def addResolutionType[Ctxt](
              toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type]
            ): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
              case TypeRep.Tree => Command.lift[Ctxt, Type](treeType)
              case other => toResolution(k)(other)
            }

            def addReification[Ctxt](
              reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              projectReiification: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type]
            ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] =
              k => rep => rep.inst match {
                case Node(id, elems) =>
                  for {
                    elems <- forEach(elems) { elem =>
                      projectReiification(k)(InstanceRep(TypeRep.Tree)(elem))
                    }
                    ident <- projectReiification(k)(InstanceRep(TypeRep.Int)(id))
                    result <- Apply[CreateNode, Expression, Expression](CreateNode(), ident +: elems).interpret(nodeCreation[Ctxt])
                  } yield result
                case Leaf(inst) =>
                  for {
                    child <- projectReiification(k)(inst)
                    childType <- projectResolution(k)(inst.tpe)
                    result <- Apply[CreateLeaf[Type], Expression, Expression](CreateLeaf(childType), Seq(child)).interpret(leafCreation[Ctxt])
                  } yield result
                case _ => reify(k)(rep)
              }

            def addExtraImport(
              importResolution: ContextSpecificResolver => Type => Option[Import]
            ): ContextSpecificResolver => Type => Option[Import] = k => {
              case tpe if tpe == treeType => AnyParadigm.guessImport(treeType)
              case other => importResolution(k)(other)
            }

            resolver.copy(
              _methodTypeResolution =
                addResolutionType(resolver._methodTypeResolution),
              _constructorTypeResolution =
                addResolutionType(resolver._constructorTypeResolution),
              _classTypeResolution =
                addResolutionType(resolver._classTypeResolution),
              _reificationInConstructor =
                addReification(
                  resolver._reificationInConstructor,
                  _.reificationInConstructor,
                  _.constructorTypeResolution
                ),
              _reificationInMethod =
                addReification(
                  resolver._reificationInMethod,
                  _.reificationInMethod,
                  _.methodTypeResolution
                ),
              _tpeImportResolution = addExtraImport(resolver._tpeImportResolution)
            ).addInfo(TreesEnabled)
          }

          implicit val dialect = scala.meta.dialects.Scala3
          val extraUnits: Seq[(Seq[scala.meta.Term.Name], Source)] = Seq(
            ((pkgName :+ ScalaNameProvider.mangle("Tree")).map(n => Term.Name(n.toAST.value)),
            s"""package ${pkgName.map(_.original).mkString(".")}
              |enum Tree {
              |  case Leaf[T](value: T)
              |  case Node(id: Int, children: Tree*)
              |}""".stripMargin.parse[Source].get)
          )

          (context.copy(resolver = updateResolver(context.resolver), units = (context.units ++ extraUnits).distinct), ())
        } else (context, ())
      }
    })
}
