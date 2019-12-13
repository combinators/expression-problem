package org.combinators.ep.language.java

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.language.java.Syntax.MangledName
import Syntax.default._
import com.github.javaparser.ast.ImportDeclaration
import org.combinators.ep.generator.Command

case class ContextSpecificResolver(
  _methodTypeResolution: ContextSpecificResolver => TypeRep => Generator[MethodBodyCtxt, Type],
  _constructorTypeResolution: ContextSpecificResolver => TypeRep => Generator[CtorCtxt, Type],
  _classTypeResolution: ContextSpecificResolver => TypeRep => Generator[ClassCtxt, Type],
  _reificationInConstructor: ContextSpecificResolver => InstanceRep => Generator[CtorCtxt, Expression],
  _reificationInMethod: ContextSpecificResolver => InstanceRep => Generator[MethodBodyCtxt, Expression],
  _importResolution: ContextSpecificResolver => Type => Option[Import],
  _instantiationOverride: ContextSpecificResolver => (Type, Seq[Expression]) => (Type, Seq[Expression]),
  generatedVariables: Map[String, MangledName]
) {
  def methodTypeResolution(tpeRep: TypeRep): Generator[MethodBodyCtxt, Type] =
    _methodTypeResolution(this)(tpeRep)
  def constructorTypeResolution(tpeRep: TypeRep): Generator[CtorCtxt, Type] =
    _constructorTypeResolution(this)(tpeRep)
  def classTypeResolution(tpeRep: TypeRep): Generator[ClassCtxt, Type] =
    _classTypeResolution(this)(tpeRep)
  def reificationInConstructor(instRep: InstanceRep): Generator[CtorCtxt, Expression] =
    _reificationInConstructor(this)(instRep)
  def reificationInMethod(instRep: InstanceRep): Generator[MethodBodyCtxt, Expression] =
    _reificationInMethod(this)(instRep)
  def importResolution(tpe: Type): Option[Import] =
    _importResolution(this)(tpe)
  def instantiationOverride(tpe: Type, args: Seq[Expression]): (Type, Seq[Expression]) =
    _instantiationOverride(this)(tpe,args)
}

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
        toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type]
      ): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
        case r if r == rep => Command.lift(targetType)
        case other => toResolution(k)(other)
      }

      def addReification[Ctxt](
        reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression]
      ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] = k => {
        case instRep if instRep.tpe == rep =>
          Command.lift(reification(instRep.inst.asInstanceOf[rep.HostType]))
        case other => reify(k)(other)
      }

      def addExtraImport(
        importResolution: ContextSpecificResolver => Type => Option[Import]
      ): ContextSpecificResolver => Type => Option[Import] = k =>  {
        case r if r == translateTo || r == possiblyBoxedTargetType(true) => extraImport
        case other => importResolution(k)(other)
      }

      resolver.copy(
        _methodTypeResolution =
          addResolutionType(
            possiblyBoxedTargetType(config.boxLevel.inMethods),
            resolver._methodTypeResolution
          ),
        _constructorTypeResolution =
          addResolutionType(
            possiblyBoxedTargetType(config.boxLevel.inConstructors),
            resolver._constructorTypeResolution
          ),
        _classTypeResolution =
          addResolutionType(
            possiblyBoxedTargetType(config.boxLevel.inClasses),
            resolver._classTypeResolution
          ),
        _reificationInConstructor = addReification(resolver._reificationInConstructor),
        _reificationInMethod = addReification(resolver._reificationInMethod),
        _importResolution = addExtraImport(resolver._importResolution)
      )
    }
}