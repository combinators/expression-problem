package org.combinators.equals.ffi.java

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.ImportDeclaration
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, InstanceRep, TypeRep, Understands}
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.{AddImport, FindClass, InstantiateObject, ToTargetLanguageType}
import org.combinators.ep.language.java.CodeGenerator.Enable
import org.combinators.ep.language.java.{ContextSpecificResolver, JavaNameProvider, ProjectCtxt, TestCtxt}
import org.combinators.ep.language.java.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.equals.ffi.BaseType as BT

trait BaseType[Ctxt, AP <: AnyParadigm] extends BT[Ctxt] {
  case object BaseTypeEnabled
  
  val base: AP
  val ooParadigm: ObjectOriented[base.type]
  import base.syntax._
  
  def enable(): Generator[base.ProjectContext, Unit] =
    Enable.interpret(using new Understands[base.ProjectContext, Enable.type] {
      def perform(
                   context: ProjectCtxt,
                   command: Enable.type
                 ): (ProjectCtxt, Unit) = {
        if (!context.resolver.resolverInfo.contains(BaseTypeEnabled)) {

          val objectName = ObjectOriented.fromComponents("java", "lang", "Object")
          val objectType = ObjectOriented.nameToType(objectName)

          def updateResolver(resolver: ContextSpecificResolver): ContextSpecificResolver = {
            def addResolutionType[Ctxt](
                   toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
                   canFindClass: Understands[Ctxt, FindClass[Name, Type]]): ContextSpecificResolver => TypeRep => Generator[Ctxt, Type] = k => {
              case BT.AnyTpe => Command.lift[Ctxt, Type](objectType)
              case BT.CompositeTpe(descriptor) => FindClass(Seq(JavaNameProvider.mangle(descriptor.name))).interpret(canFindClass)
              case other => toResolution(k)(other)
            }

            def addReification[Ctxt](
                    reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
                    projectReify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression],
                    toTargetLanguageType: ContextSpecificResolver => TypeRep => Generator[Ctxt, Type],
                    canConstructObject: Understands[Ctxt, InstantiateObject[Type, Expression, ooParadigm.ClassContext]]
                  ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] =
              k => rep => rep.tpe match {

                case BT.AnyTpe =>
                  for {
                    result <- InstantiateObject(objectType, Seq.empty).interpret(canConstructObject)
                  } yield result

                case ct: BT.CompositeTpe =>
                  for {
                    translatedTpe <- toTargetLanguageType(k)(rep.tpe)
                    translatedArgs <- forEach(rep.inst.asInstanceOf[Seq[(String, InstanceRep)]]){ case (name, fieldRep) =>
                      projectReify(k)(fieldRep)
                    }
                    result <- InstantiateObject(translatedTpe, translatedArgs).interpret(canConstructObject)
                  } yield result
               
                case _ => reify(k)(rep)
              }

            resolver.copy(
              _methodTypeResolution = addResolutionType(resolver._methodTypeResolution, ooParadigm.methodBodyCapabilities.canFindClassInMethod),
              _constructorTypeResolution = addResolutionType(resolver._constructorTypeResolution, ooParadigm.constructorCapabilities.canFindClassInConstructor),
              _classTypeResolution =
                addResolutionType(resolver._classTypeResolution, ooParadigm.classCapabilities.canFindClassInClass),
              _reificationInConstructor =
                addReification(resolver._reificationInConstructor, _.reificationInConstructor, _.constructorTypeResolution, ooParadigm.constructorCapabilities.canInstantiateObjectInConstructor),
              _reificationInMethod =
                addReification(resolver._reificationInMethod, _.reificationInMethod, _.methodTypeResolution, ooParadigm.methodBodyCapabilities.canInstantiateObjectInMethod),
            ).addInfo(BaseTypeEnabled)
          }

          (context.copy(resolver = updateResolver(context.resolver)), ())
        } else (context, ())
      }
    })
}

object BaseType {
  type Aux[Ctxt, AP <: AnyParadigm, OO <: ObjectOriented[AP]] = BaseType[Ctxt, AP] {
    val ooParadigm: OO
  }
  def apply[Ctxt, AP <: AnyParadigm, OO[A <: AP] <: ObjectOriented[A]](
            base: AP)
          (ooParadigm: OO[base.type]): Aux[Ctxt, base.type, ooParadigm.type] = {
    val b: base.type = base
    val oo: ooParadigm.type = ooParadigm
    case class T(
                  val base: b.type,
                  val ooParadigm: oo.type
                ) extends BaseType[Ctxt, b.type]

    T(b, oo)
  }
}