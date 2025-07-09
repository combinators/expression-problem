package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import java.nio.file.Paths
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.{ImportDeclaration, NodeList}
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.expr.ObjectCreationExpr
import org.combinators.cogen.InstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AddImport, Apply}
import org.combinators.ep.domain.tree.{Leaf, Node, Tree}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.ffi.{Create, CreateLeaf, CreateNode, Trees as Ts}
import org.combinators.ep.domain.abstractions.DomainTpeRep
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.{ContextSpecificResolver, ProjectCtxt}
import org.combinators.ep.language.java.paradigm.{AnyParadigm, Generics, ObjectOriented}
import org.combinators.ep.language.java.Syntax.default.*

trait Trees[Ctxt, AP <: AnyParadigm] extends Ts[Ctxt] {
  case object TreesEnabled

  val base: AP
  val addImport: Understands[Ctxt, AddImport[Import]]
  val ooParadigm: ObjectOriented[base.type]

  val treeImport = new ImportDeclaration("org.combinators.ep.util.Tree", false, false)
  val leafImport = new ImportDeclaration("org.combinators.ep.util.Leaf", false, false)
  val nodeImport = new ImportDeclaration("org.combinators.ep.util.Node", false, false)

  def leafCreation[Ctxt](canAddImport: Understands[Ctxt, AddImport[Import]]): Understands[Ctxt, Apply[CreateLeaf[Type], Expression, Expression]] =
    new Understands[Ctxt, Apply[CreateLeaf[Type], Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[CreateLeaf[Type], Expression, Expression]
      ): (Ctxt, Expression) = {
        val leafType = ObjectOriented.nameToType(leafImport.getName)
        leafType.setTypeArguments(new NodeList[Type](command.functional.valueType))
        val gen =
          for {
            _ <- AddImport(leafImport).interpret(canAddImport)
          } yield
            new ObjectCreationExpr(
              null,
              leafType,
              new NodeList[Type](),
              new NodeList[Expression](command.arguments.head),
              null
            )
        Command.runGenerator(gen, context)
      }
    }

  def nodeCreation[Ctxt](canAddImport: Understands[Ctxt, AddImport[Import]]): Understands[Ctxt, Apply[CreateNode, Expression, Expression]] =
    new Understands[Ctxt, Apply[CreateNode, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[CreateNode, Expression, Expression]
      ): (Ctxt, Expression) = {
        val gen =
          for {
            _ <- AddImport(nodeImport).interpret(canAddImport)
          } yield
            new ObjectCreationExpr(
              null,
              ObjectOriented.nameToType(nodeImport.getName),
              new NodeList[Expression](command.arguments*)
            )
        Command.runGenerator(gen, context)
      }
    }

  override val treeCapabilities: TreeCapabilities =
    new TreeCapabilities {
      implicit val canCreateLeaf: Understands[Ctxt, Apply[CreateLeaf[Type], Expression, Expression]] =
        leafCreation(addImport)
      implicit val canCreateNode: Understands[Ctxt, Apply[CreateNode, Expression, Expression]] =
        nodeCreation(addImport)
    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        if (!context.resolver.resolverInfo.contains(TreesEnabled)) {

          val treeType = ObjectOriented.nameToType(treeImport.getName)

          def updateResolver(resolver: ContextSpecificResolver): ContextSpecificResolver = {
            def addResolutionType[Ctxt](
              toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              canAddImport: Understands[Ctxt, AddImport[Import]]
            ): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
              case DomainTpeRep.Tree =>
                for {
                  _ <- AddImport(treeImport).interpret(canAddImport)
                } yield treeType
              case other => toResolution(k)(other)
            }

            def addReification[Ctxt](
              reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              projectReiification: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              canAddImport: Understands[Ctxt, AddImport[Import]]
            ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] =
              k => rep => rep.inst match {
                case Node(id, elems) =>
                  for {
                    elems <- forEach(elems) { elem =>
                      projectReiification(k)(InstanceRep(DomainTpeRep.Tree)(elem))
                    }
                    ident <- projectReiification(k)(InstanceRep(TypeRep.Int)(id))
                    result <- Apply[CreateNode, Expression, Expression](CreateNode(), ident +: elems).interpret(nodeCreation(canAddImport))
                  } yield result
                case Leaf(inst) =>
                  for {
                    child <- projectReiification(k)(inst)
                    childType <- projectResolution(k)(inst.tpe)
                    result <- Apply[CreateLeaf[Type], Expression, Expression](CreateLeaf(childType), Seq(child)).interpret(leafCreation(canAddImport))
                  } yield result
                case _ => reify(k)(rep)
              }

            def addExtraImport(
              importResolution: ContextSpecificResolver => Type => Option[Import]
            ): ContextSpecificResolver => Type => Option[Import] = k => {
              case tpe
                if tpe
                  .toClassOrInterfaceType
                  .map[Boolean](clsTy => clsTy.getNameWithScope == treeType.asClassOrInterfaceType().getNameWithScope)
                  .orElse(false) =>
                Some(treeImport)
              case other => importResolution(k)(other)
            }

            resolver.copy(
              _methodTypeResolution =
                addResolutionType(
                  resolver._methodTypeResolution,
                  base.methodBodyCapabilities.canAddImportInMethodBody
                ),
              _constructorTypeResolution =
                addResolutionType(
                  resolver._constructorTypeResolution,
                  ooParadigm.constructorCapabilities.canAddImportInConstructor
                ),
              _classTypeResolution =
                addResolutionType(
                  resolver._classTypeResolution,
                  ooParadigm.classCapabilities.canAddImportInClass
                ),
              _reificationInConstructor =
                addReification(
                  resolver._reificationInConstructor,
                  _.reificationInConstructor,
                  _.constructorTypeResolution,
                  ooParadigm.constructorCapabilities.canAddImportInConstructor
                ),
              _reificationInMethod =
                addReification(
                  resolver._reificationInMethod,
                  _.reificationInMethod,
                  _.methodTypeResolution,
                  base.methodBodyCapabilities.canAddImportInMethodBody
                ),
              _importResolution = addExtraImport(resolver._importResolution)
            ).addInfo(TreesEnabled)
          }

          val extraUnits: Seq[CompilationUnit] =
            Seq(
              "Leaf.java",
              "Node.java",
              "Tree.java"
            ).map(fileName =>
              StaticJavaParser.parse(getClass.getResourceAsStream(s"/java-code/org/combinators/ep/util/$fileName"))
            )

          (context.copy(resolver = updateResolver(context.resolver), units = (context.units ++ extraUnits).distinct), ())
        } else (context, ())
      }
    })
}

object Trees {
  type Aux[Ctxt, AP <: AnyParadigm, OO <: ObjectOriented[AP]] = Trees[Ctxt, AP] {
    val ooParadigm: OO
  }
  def apply[Ctxt, AP <: AnyParadigm, OO[A <: AP] <: ObjectOriented[A]](
    base: AP,
    addImport: Understands[Ctxt, AddImport[Import]])
    (ooParadigm: OO[base.type]): Aux[Ctxt, base.type, ooParadigm.type] = {
    val b: base.type = base
    val addImp = addImport
    val oo: ooParadigm.type = ooParadigm
    case class T(
      val base: b.type,
      val addImport: Understands[Ctxt, AddImport[Import]],
      val ooParadigm: oo.type
    ) extends Trees[Ctxt, b.type]

    T(b, addImport, oo)
  }
}
