package org.combinators.equals.ffi.scala    /*DI:LD:AI*/

import org.combinators.cogen.{InstanceRep, TypeRep}
import org.combinators.equals.ffi.inbetween.BaseTypeAST as InbetweenBaseTypeAST
import org.combinators.ep.language.inbetween.oo.OOAST
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST, FinalNameProviderAST, NameProviderAST}
import org.combinators.equals.CompositeDataType
import org.combinators.equals.ffi.BaseType

trait BaseTypeAST extends InbetweenBaseTypeAST { self: OOAST & NameProviderAST & BaseAST =>
  object scalaBaseTypeOps {
    object baseTypeOpsOverride {
      trait FinalTypes extends baseTypeOps.FinalTypes {
        type BaseType <: baseTypeOpsOverride.BaseType
        type CompositeType <: baseTypeOpsOverride.CompositeType
      }
      
      trait BaseType extends baseTypeOps.BaseType with scalaBase.ooOverrides.ClassReferenceType {
        def qualifiedClassName: Seq[any.Name] = qualifiedClassNameAny
      }

      trait CompositeType extends baseTypeOps.CompositeType with scalaBase.ooOverrides.ClassReferenceType {
        def qualifiedClassName: Seq[any.Name] = Seq(nameProviderFactory.scalaNameProvider.mangle(model.name))
        import factory._
        import baseTypeOpsFactory._
        override def copy(qualifiedClassName:Seq[any.Name]) = copyAsCompositeType(model.copy(name = qualifiedClassName.last.component))
      }

      trait Factory extends baseTypeOps.Factory {}
    }

    val qualifiedClassNameAny: Seq[any.Name] = Seq("Any").map(n => scalaBaseFactory.name(n, n))

    def baseTypePrefixExcludes: Set[Seq[any.Name]] =
      Set(qualifiedClassNameAny)

    def baseTypeReificationExtensions(tpe: TypeRep)(value: tpe.HostType): Option[String] = {
      import baseTypeOpsFactory._
      tpe match {
        case BaseType.AnyTpe => Some(s"new java.lang.Object")
        case BaseType.CompositeTpe(description) =>
          val args = value.asInstanceOf[Seq[(String, InstanceRep)]].map({ (name, instRep) =>
            s"${scalaBaseFactory.reifiedScalaValue(instRep.tpe, instRep.inst).toScala}"     // ${name} =   cannot work since constructor params are mangled
          })
          Some(s"new ${compositeType(description).toScala}${args.mkString("(", ", ", ")")}")
        case _ => None
      }
    }
  }

  override val baseTypeOpsFinalTypes: scalaBaseTypeOps.baseTypeOpsOverride.FinalTypes
  override val baseTypeOpsFactory: scalaBaseTypeOps.baseTypeOpsOverride.Factory
}

trait FinalBaseTypeAST extends BaseTypeAST { self: NameProviderAST & FinalBaseAST =>
  object baseTypeFinalTypes {
    trait FinalBaseType extends scalaBaseTypeOps.baseTypeOpsOverride.FinalTypes {
      type BaseType = scalaBaseTypeOps.baseTypeOpsOverride.BaseType
      type CompositeType = scalaBaseTypeOps.baseTypeOpsOverride.CompositeType
    }
  }

  object finalBaseTypeFactoryTypes {
    trait FinalBaseTypeFactory extends scalaBaseTypeOps.baseTypeOpsOverride.Factory {
      def baseType(): baseTypeOps.BaseType = {
        case class BaseType() extends scalaBaseTypeOps.baseTypeOpsOverride.BaseType {
          override def getSelfBaseType: baseTypeOpsFinalTypes.BaseType = this
          override def getSelfClassReferenceType: scalaBase.ooOverrides.ClassReferenceType = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        BaseType()
      }

      def compositeType(model:CompositeDataType): baseTypeOps.CompositeType = {
        class CompositeType(override val model:CompositeDataType) extends scalaBaseTypeOps.baseTypeOpsOverride.CompositeType {
          override def getSelfCompositeType: baseTypeOpsFinalTypes.CompositeType = this
          def getSelfClassReferenceType: scalaBase.ooOverrides.ClassReferenceType = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        CompositeType(model)
      }
    }
  }

  override val baseTypeOpsFinalTypes: baseTypeFinalTypes.FinalBaseType = new baseTypeFinalTypes.FinalBaseType {}
  override val baseTypeOpsFactory: finalBaseTypeFactoryTypes.FinalBaseTypeFactory = new finalBaseTypeFactoryTypes.FinalBaseTypeFactory {}
}