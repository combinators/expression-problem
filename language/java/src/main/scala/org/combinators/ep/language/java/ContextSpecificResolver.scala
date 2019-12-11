package org.combinators.ep.language.java

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.language.java.Syntax.MangledName
import Syntax.default._
import com.github.javaparser.ast.ImportDeclaration
import org.combinators.ep.generator.Command

case class ContextSpecificResolver(
  _methodTypeResolution: (TypeRep => Generator[MethodBodyCtxt, Type]) => TypeRep => Generator[MethodBodyCtxt, Type],
  _constructorTypeResolution: (TypeRep => Generator[CtorCtxt, Type]) => TypeRep => Generator[CtorCtxt, Type],
  _classTypeResolution: (TypeRep => Generator[ClassCtxt, Type]) => TypeRep => Generator[ClassCtxt, Type],
  _reificationInConstructor: (InstanceRep => Generator[CtorCtxt, Expression]) => InstanceRep => Generator[CtorCtxt, Expression],
  _reificationInMethod: (InstanceRep => Generator[MethodBodyCtxt, Expression]) => InstanceRep => Generator[MethodBodyCtxt, Expression],
  _importResolution: (Type => Option[Import]) => Type => Option[Import],
  _instantiationOverride: ((Type, Seq[Expression]) => (Type, Seq[Expression])) => (Type, Seq[Expression]) => (Type, Seq[Expression]),
  generatedVariables: Map[String, MangledName]
) {
  def methodTypeResolution(tpeRep: TypeRep): Generator[MethodBodyCtxt, Type] =
    _methodTypeResolution(methodTypeResolution)(tpeRep)
  def constructorTypeResolution(tpeRep: TypeRep): Generator[CtorCtxt, Type] =
    _constructorTypeResolution(constructorTypeResolution)(tpeRep)
  def classTypeResolution(tpeRep: TypeRep): Generator[ClassCtxt, Type] =
    _classTypeResolution(classTypeResolution)(tpeRep)
  def reificationInConstructor(instRep: InstanceRep): Generator[CtorCtxt, Expression] =
    _reificationInConstructor(reificationInConstructor)(instRep)
  def reificationInMethod(instRep: InstanceRep): Generator[MethodBodyCtxt, Expression] =
    _reificationInMethod(reificationInMethod)(instRep)
  def importResolution(tpe: Type): Option[Import] =
    _importResolution(importResolution)(tpe)
  def instantiationOverride(tpeAndArgs: (Type, Seq[Expression])): (Type, Seq[Expression])
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
        toResolution: (TypeRep => Generator[Ctxt, Type]) => TypeRep => Generator[Ctxt, Type]
      ): (TypeRep => Generator[Ctxt, Type]) => TypeRep => Generator[Ctxt, Type] = k => {
        case r if r == rep => Command.lift(targetType)
        case other => toResolution(k)(other)
      }

      def addReification[Ctxt](
        reify: (InstanceRep => Generator[Ctxt, Expression]) => InstanceRep => Generator[Ctxt, Expression]
      ): (InstanceRep => Generator[Ctxt, Expression]) => InstanceRep => Generator[Ctxt, Expression] = k => {
        case instRep if instRep.tpe == rep =>
          Command.lift(reification(instRep.inst.asInstanceOf[rep.HostType]))
        case other => reify(k)(other)
      }

      def addExtraImport(
        importResolution: (Type => Option[Import]) => Type => Option[Import]
      ): (Type => Option[Import]) => Type => Option[Import] = k =>  {
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