package org.combinators.ep.language.java

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.language.java.Syntax.MangledName
import Syntax.default._
import com.github.javaparser.ast.ImportDeclaration
import org.combinators.ep.generator.Command

case class ContextSpecificResolver(
  methodTypeResolution: TypeRep => Generator[MethodBodyCtxt, Type],
  constructorTypeResolution: TypeRep => Generator[CtorCtxt, Type],
  classTypeResolution: TypeRep => Generator[ClassCtxt, Type],
  reificationInConstructor: InstanceRep => Generator[CtorCtxt, Expression],
  reificationInMethod: InstanceRep => Generator[MethodBodyCtxt, Expression],
  importResolution: Type => Option[Import],
  instantiationOverride: (Type, Seq[Expression]) => (Type, Seq[Expression]),
  generatedVariables: Map[String, MangledName]
)

object ContextSpecificResolver {
  def updateResolver
    (config: Config, rep: TypeRep, translateTo: Type, extraImport: Option[ImportDeclaration] = None)
    (reification: rep.HostType => Expression): ContextSpecificResolver => ContextSpecificResolver =
    resolver => {
      def possiblyBoxedTargetType(needsBox: Boolean): Type = {
        if (needsBox && translateTo.isPrimitiveType) {
          translateTo.asPrimitiveType().toBoxedType
        } else translateTo
      }

      def addResolutionType[Ctxt](
        targetType: Type,
        toResolution: TypeRep => Generator[Ctxt, Type]
      ): TypeRep => Generator[Ctxt, Type] = {
        case r if r == rep => Command.lift(targetType)
        case other => toResolution(other)
      }

      def addReification[Ctxt](
        reify: InstanceRep => Generator[Ctxt, Expression]
      ): InstanceRep => Generator[Ctxt, Expression] = {
        case instRep if instRep.tpe == rep =>
          Command.lift(reification(instRep.inst.asInstanceOf[rep.HostType]))
        case other => reify(other)
      }

      def addExtraImport(
        importResolution: Type => Option[Import]
      ): Type => Option[Import] = {
        case r if r == translateTo || r == possiblyBoxedTargetType(true) => extraImport
        case other => importResolution(other)
      }

      resolver.copy(
        methodTypeResolution =
          addResolutionType(
            possiblyBoxedTargetType(config.boxLevel.inMethods),
            resolver.methodTypeResolution
          ),
        constructorTypeResolution =
          addResolutionType(
            possiblyBoxedTargetType(config.boxLevel.inConstructors),
            resolver.constructorTypeResolution
          ),
        classTypeResolution =
          addResolutionType(
            possiblyBoxedTargetType(config.boxLevel.inClasses),
            resolver.classTypeResolution
          ),
        reificationInConstructor = addReification(resolver.reificationInConstructor),
        reificationInMethod = addReification(resolver.reificationInMethod),
        importResolution = addExtraImport(resolver.importResolution)
      )
    }
}