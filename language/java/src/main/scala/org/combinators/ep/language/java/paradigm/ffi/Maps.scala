package org.combinators.ep.language.java.paradigm.ffi    /*DI:LD:AI*/

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.InitializerDeclaration
import com.github.javaparser.ast.expr.{MethodCallExpr, ObjectCreationExpr, ThisExpr, TypeExpr}
import com.github.javaparser.ast.stmt.{BlockStmt, ExpressionStmt}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, InstanceRep, TypeRep, Understands}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.{ContextSpecificResolver, ProjectCtxt}
import org.combinators.ep.language.java.paradigm.{AnyParadigm, Generics, ObjectOriented}
import org.combinators.ep.language.java.Syntax.default.*
import org.combinators.cogen.paradigm.AnyParadigm.syntax.*
import com.github.javaparser.ast.{ImportDeclaration, NodeList}
import org.combinators.cogen.paradigm.ffi.{CreateMap, ContainsKey, GetOrElse, Put, Maps as Mps}
import org.combinators.cogen.paradigm.{AddImport, Apply}

trait Maps[Ctxt, AP <: AnyParadigm] extends Mps[Ctxt] {
  case object MapsEnabled

  val base: AP

  val generics: Generics[base.type]
  val canAddImport: Understands[Ctxt, AddImport[Import]]

  val mapImp = new ImportDeclaration("java.util.Map", false, false)
  val hashMapImp = new ImportDeclaration("java.util.HashMap", false, false)

  def createMap[Ctxt](canAddImport : Understands[Ctxt, AddImport[Import]]) : Understands[Ctxt, Apply[CreateMap[Type], (Expression,Expression), Expression]] =
    new Understands[Ctxt, Apply[CreateMap[Type], (Expression,Expression), Expression]] {
      def perform(
                   context: Ctxt,
                   command: Apply[CreateMap[Type], (Expression,Expression), Expression]
                 ): (Ctxt, Expression) = {

        val mapType = ObjectOriented.nameToType(mapImp.getName)

        val classScope = new TypeExpr(mapType)
        val entries = command.arguments.map({
          case (key, value) =>
            new MethodCallExpr(classScope, "entry", new NodeList(key, value))
        })
        val gen = for {
          _ <- AddImport(mapImp).interpret(canAddImport)
        } yield new MethodCallExpr(
          classScope,
          new NodeList(command.functional.keyType, command.functional.elementType),
          "ofEntries",
          new NodeList(entries*))

        Command.runGenerator(gen, context)
      }
    }

  def putIntoMap[Ctxt](canAddImport : Understands[Ctxt, AddImport[Import]]) : Understands[Ctxt, Apply[Put, Expression, Expression]] =
    new Understands[Ctxt, Apply[Put, Expression, Expression]] {
      override def perform(
                            context: Ctxt,
                            command: Apply[Put, Expression, Expression]
                          ): (Ctxt, Expression) = {
        val hashMapType = ObjectOriented.nameToType(hashMapImp.getName)
        val mapType = ObjectOriented.nameToType(mapImp.getName)
        val methodCallExpression = new MethodCallExpr(
          new ThisExpr(),
          "put",
          new NodeList[Expression](command.arguments(1), command.arguments(2))
        )

        val inits = new ExpressionStmt(methodCallExpression)
        val blockStmt = new BlockStmt(new NodeList[Statement](inits))
        val bodyDecl = new InitializerDeclaration(false, blockStmt)

        val gen = for {
          _ <- AddImport(hashMapImp).interpret(canAddImport)
        } yield new ObjectCreationExpr(
          null,
          hashMapType,
          new NodeList[Type](),
          new NodeList[Expression](command.arguments.head),
          new NodeList(bodyDecl)
        )

        Command.runGenerator(gen, context)
      }
    }

  val mapCapabilities: MapCapabilities =
    new MapCapabilities {

      implicit val canCreate: Understands[Ctxt, Apply[CreateMap[Type], (Expression,Expression), Expression]] = createMap(canAddImport)

      implicit val canContainsKey: Understands[Ctxt, Apply[ContainsKey, Expression, Expression]] =
        new Understands[Ctxt, Apply[ContainsKey, Expression, Expression]] {
          def perform(
                       context: Ctxt,
                       command: Apply[ContainsKey, Expression, Expression]
                     ): (Ctxt, Expression) = {
            (context, new MethodCallExpr(command.arguments(0), "containsKey", new NodeList[Expression](command.arguments(1))))
          }
        }

      implicit val canGet: Understands[Ctxt, Apply[GetOrElse, Expression, Expression]] =
        new Understands[Ctxt, Apply[GetOrElse, Expression, Expression]] {
          def perform(
            context: Ctxt,
            command: Apply[GetOrElse, Expression, Expression]
          ): (Ctxt, Expression) = {
            (context, new MethodCallExpr(command.arguments(0), "getOrDefault", new NodeList[Expression](command.arguments(1), command.arguments(2))))
          }
        }

      implicit val canPut: Understands[Ctxt, Apply[Put, Expression, Expression]] = putIntoMap(canAddImport)

    }

  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(using new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        if (!context.resolver.resolverInfo.contains(MapsEnabled)) {

          val mapName = ObjectOriented.fromComponents("java", "util", "Map")
          val mapType = ObjectOriented.nameToType(mapName)
          val mapImp = new ImportDeclaration(mapName, false, false)

          val hashMapName = ObjectOriented.fromComponents("java", "util", "HashMap")
          val hashMapType = ObjectOriented.nameToType(hashMapName)
          val hashMapImp = new ImportDeclaration(hashMapName, false, false)

          def updateResolver(resolver: ContextSpecificResolver): ContextSpecificResolver = {
            def addResolutionType[Ctxt](
              toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              canApplyType: Understands[Ctxt, Apply[Type, Type, Type]]
            ): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
              case TypeRep.Map(keyRep, elemRep) =>
                for {
                  keyType <- projectResolution(k)(keyRep)
                  elemType <- projectResolution(k)(elemRep)
                  resultType <- Apply[Type, Type, Type](mapType, Seq(keyType, elemType)).interpret(using canApplyType)
                } yield resultType
              case other => toResolution(k)(other)
            }

            def addReification[Ctxt](
              reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              projectResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
              projectReiification: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
              canCreateMap: Understands[Ctxt, Apply[CreateMap[Type], (Expression, Expression), Expression]]
            ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] =
              k => rep => rep.tpe match {
                case TypeRep.Map(keyTypeRep, elemTypeRep) =>
                  for {
                    elems <- forEach(rep.inst.asInstanceOf[Map[keyTypeRep.HostType, elemTypeRep.HostType]].toSeq) { case (key,value) =>
                      for {
                        reifiedKey <- projectReiification(k) (InstanceRep(keyTypeRep) (key))
                        reifiedValue <- projectReiification(k) (InstanceRep(elemTypeRep) (value))
                      } yield (reifiedKey, reifiedValue)
                    }
                    keyType <- projectResolution(k)(keyTypeRep)
                    elemType <- projectResolution(k)(elemTypeRep)
                    res <- Apply[CreateMap[Type], (Expression, Expression), Expression](CreateMap(keyType, elemType), elems).interpret(using canCreateMap)
                  } yield res
                case _ => reify(k)(rep)
              }

            def addExtraImport(
              importResolution: ContextSpecificResolver => Type => Option[Import]
            ): ContextSpecificResolver => Type => Option[Import] = k => {
              case tpe
                if AnyParadigm.stripGenerics(tpe)
                  .toClassOrInterfaceType
                  .map[Boolean](clsTy => clsTy.getNameWithScope == mapType.asClassOrInterfaceType().getNameWithScope)
                  .orElse(false) =>
                Some(mapImp)

              case tpe
                if AnyParadigm.stripGenerics(tpe)
                  .toClassOrInterfaceType
                  .map[Boolean](clsTy => clsTy.getNameWithScope == hashMapType.asClassOrInterfaceType().getNameWithScope)
                  .orElse(false) =>
                Some(hashMapImp)

              case other => importResolution(k)(other)
            }

            resolver.copy(
              _methodTypeResolution =
                addResolutionType(
                  resolver._methodTypeResolution,
                  _.methodTypeResolution,
                  generics.ppolyParadigm.methodBodyCapabilities.canApplyTypeInMethod
                ),
              _constructorTypeResolution =
                addResolutionType(
                  resolver._constructorTypeResolution,
                  _.constructorTypeResolution,
                  generics.constructorCapabilities.canApplyTypeInConstructor
                ),
              _classTypeResolution =
                addResolutionType(
                  resolver._classTypeResolution,
                  _.classTypeResolution,
                  generics.classCapabilities.canApplyTypeInClass
                ),
              _reificationInConstructor =
                addReification(
                  resolver._reificationInConstructor,
                  _.constructorTypeResolution,
                  _.reificationInConstructor,
                  createMap(generics.ooParadigm.constructorCapabilities.canAddImportInConstructor)
                ),
              _reificationInMethod =
                addReification(
                  resolver._reificationInMethod,
                  _.methodTypeResolution,
                  _.reificationInMethod,
                  createMap(base.methodBodyCapabilities.canAddImportInMethodBody)
                ),
              _importResolution = addExtraImport(resolver._importResolution)
            ).addInfo(MapsEnabled)
          }


          (context.copy(resolver = updateResolver(context.resolver)), ())
        } else (context, ())
      }
    })
}

object Maps {
  type Aux[Ctxt, AP <: AnyParadigm, Gen <: Generics[AP]] = Maps[Ctxt, AP] {
    val generics: Gen
  }
  def apply[Ctxt, AP <: AnyParadigm, Gen[A <: AP] <: Generics[A]](
     base: AP,
     addImport: Understands[Ctxt, AddImport[Import]])
     (
     generics: Gen[base.type]
   ): Aux[Ctxt, base.type, generics.type] = {
    val b: base.type = base
    val addImp = addImport
    val gen: generics.type = generics

    case class Mps(override val base: b.type,
                   override val canAddImport: Understands[Ctxt, AddImport[Import]],
                   override val generics: gen.type
                   ) extends Maps[Ctxt, b.type]

    Mps(b, addImp, gen)
  }
}