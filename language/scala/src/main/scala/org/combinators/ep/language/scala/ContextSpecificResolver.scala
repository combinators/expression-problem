package org.combinators.ep.language.scala

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command
import org.combinators.ep.generator.Command.Generator

import scala.meta.Term
import scala.meta.Import

import Syntax.default._

case class ContextSpecificResolver(
  _methodTypeResolution: ContextSpecificResolver => TypeRep => Generator[MethodBodyCtxt, Type],
  _constructorTypeResolution: ContextSpecificResolver => TypeRep => Generator[CtorCtxt, Type],
  _classTypeResolution: ContextSpecificResolver => TypeRep => Generator[ClassCtxt, Type],
  _reificationInConstructor: ContextSpecificResolver => InstanceRep => Generator[CtorCtxt, Term],
  _reificationInMethod: ContextSpecificResolver => InstanceRep => Generator[MethodBodyCtxt, Term],
  _importResolution: ContextSpecificResolver => Type => Option[Import],
  _instantiationOverride: ContextSpecificResolver => (Type, Seq[Term]) => (Type, Seq[Term])
) {
  def methodTypeResolution(tpeRep: TypeRep): Generator[MethodBodyCtxt, Type] =
    _methodTypeResolution(this)(tpeRep)
  def constructorTypeResolution(tpeRep: TypeRep): Generator[CtorCtxt, Type] =
    _constructorTypeResolution(this)(tpeRep)
  def classTypeResolution(tpeRep: TypeRep): Generator[ClassCtxt, Type] =
    _classTypeResolution(this)(tpeRep)
  def reificationInConstructor(instRep: InstanceRep): Generator[CtorCtxt, Term] =
    _reificationInConstructor(this)(instRep)
  def reificationInMethod(instRep: InstanceRep): Generator[MethodBodyCtxt, Term] =
    _reificationInMethod(this)(instRep)
  def importResolution(tpe: Type): Option[Import] =
    _importResolution(this)(tpe)
  def instantiationOverride(tpe: Type, args: Seq[Term]): (Type, Seq[Term]) =
    _instantiationOverride(this)(tpe,args)
}

object ContextSpecificResolver {
  def updateResolver
    (config: Config, rep: TypeRep, translateTo: Type, extraImport: Option[Import] = None)
      (reification: rep.HostType => Term): ContextSpecificResolver => ContextSpecificResolver =
    resolver => {
      def addResolutionType[Ctxt](
        targetType: Type,
        toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type]
      ): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
        case r if r == rep => Command.lift(targetType)
        case other => toResolution(k)(other)
      }

      def addReification[Ctxt](
        reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Term]
      ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Term] = k => {
        case instRep if instRep.tpe == rep =>
          Command.lift(reification(instRep.inst.asInstanceOf[rep.HostType]))
        case other => reify(k)(other)
      }

      def addExtraImport(
        importResolution: ContextSpecificResolver => Type => Option[Import]
      ): ContextSpecificResolver => Type => Option[Import] = k =>  {
        case r if r == translateTo => extraImport
        case other => importResolution(k)(other)
      }

      resolver.copy(
        _methodTypeResolution =
          addResolutionType(
            translateTo,
            resolver._methodTypeResolution
          ),
        _constructorTypeResolution =
          addResolutionType(
            translateTo,
            resolver._constructorTypeResolution
          ),
        _classTypeResolution =
          addResolutionType(
            translateTo,
            resolver._classTypeResolution
          ),
        _reificationInConstructor = addReification(resolver._reificationInConstructor),
        _reificationInMethod = addReification(resolver._reificationInMethod),
        _importResolution = addExtraImport(resolver._importResolution)
      )
    }
}