package org.combinators.ep.language

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.abstractions.TypeRep.OfHostType
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.oo
import org.combinators.ep.language.inbetween.imperative
import org.combinators.ep.language.inbetween.ffi.arithmetic
import org.combinators.ep.language.inbetween.ffi.realArithmetic
import org.combinators.ep.language.inbetween.ffi.assertions
import org.combinators.ep.language.inbetween.ffi.boolean
import org.combinators.ep.language.inbetween.ffi.eqls
import org.combinators.ep.language.inbetween.ffi.operatorExpression
import org.combinators.ep.language.inbetween.ffi.strings
import org.combinators.ep.language.inbetween.ffi.lists
import org.combinators.ep.language.inbetween.ffi.trees
import org.combinators.ep.language.inbetween.polymorphism.generics
import org.combinators.ep.language.inbetween.polymorphism
import org.combinators.ep.language.inbetween.oo.{Class, Constructor}

import java.util.UUID

package object scala {
  trait FinalTypes
    extends any.FinalTypes
      with oo.FinalTypes
      with imperative.FinalTypes
      with operatorExpression.FinalTypes
      with lists.FinalTypes
      with trees.FinalTypes
      with generics.FinalTypes {
    type ReifiedScalaValue[T] <: Expression
  }

  trait Project[FT <: FinalTypes] extends oo.Project[FT] with Factory[FT] {
    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.Project[FT] =
      copyAsProjectWithTypeLookups(
        compilationUnits = compilationUnits.map(cu => convert(cu).prefixRootPackage(rootPackageName, excludedTypeNames)),
        methodTypeLookupMap = tpeRep => methodTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        constructorTypeLookupMap = tpeRep => constructorTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        classTypeLookupMap = tpeRep => classTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
      )
  }

  trait Import[FT <: FinalTypes] extends any.Import[FT] with Factory[FT] {
    def components: Seq[any.Name[FT]]

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.Import[FT] = {
      if (excludedTypeNames.contains(components)) {
        this
      } else {
        copy(components = rootPackageName ++ components)
      }
    }

    def toScala: String =
      s"""import ${components.map(_.toScala).mkString(".")}"""

    def copy(components: Seq[any.Name[FT]] = this.components): any.Import[FT] =
      importStatement(components)
  }

  trait Statement[FT <: FinalTypes] extends any.Statement[FT] with Factory[FT] {
    def toScala: String

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.Statement[FT]
  }

  trait DeclareVariable[FT <: FinalTypes] extends imperative.DeclareVariable[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = {
      val init = this.initializer.map(ie => s" = ${ie.toScala}").getOrElse("")
      s"""
         |var ${this.name.toScala}: ${this.tpe.toScala}${init}
         |""".stripMargin
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): imperative.DeclareVariable[FT] = {
      copy(
        tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
        initializer = initializer.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
      )
    }
  }

  trait AssignVariable[FT <: FinalTypes] extends imperative.AssignVariable[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = {
      s"""
         |${this.variable.toScala} = ${this.assignmentExpression.toScala}
         |""".stripMargin
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): imperative.AssignVariable[FT] =
      copy(assignmentExpression = assignmentExpression.prefixRootPackage(rootPackageName, excludedTypeNames))
  }

  trait IfThenElse[FT <: FinalTypes] extends imperative.IfThenElse[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = {
      val elseIfs = elseIfBranches.map{ case (condition, body) =>
        s"""
           | else if (${condition.toScala}) {
           |  ${body.map(_.toScala).mkString("\n  ")}
           |}""".stripMargin
      }

      s"""
          |if (${condition.toScala}) {
          |  ${this.ifBranch.map(_.toScala).mkString("\n  ")}
          |}${elseIfs.mkString("")} else {
          |  ${elseBranch.map(_.toScala).mkString("\n  ")}
          |}
          """
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): imperative.IfThenElse[FT] =
      copy(
        condition = condition.prefixRootPackage(rootPackageName, excludedTypeNames),
        ifBranch = ifBranch.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        elseIfBranches = elseIfBranches.map { case (cond, branch) =>
          (cond.prefixRootPackage(rootPackageName, excludedTypeNames),
            branch.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)))
        },
        elseBranch = elseBranch.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
      )
  }

  trait While[FT <: FinalTypes] extends imperative.While[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = {
      s"""
         |while (${condition.toScala}) {
         |  ${body.map(_.toScala).mkString("\n  ")}
         |}""".stripMargin
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): imperative.While[FT] =
      copy(
        condition = condition.prefixRootPackage(rootPackageName, excludedTypeNames),
        body = body.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
      )
  }

  trait Expression[FT <: FinalTypes] extends oo.Expression[FT] with Factory[FT] {
    def toScala: String

    def isTypeReferenceExpression: Boolean = false

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.Expression[FT]
  }

  trait ReifiedScalaValue[FT <: FinalTypes, T] extends Expression[FT] with Factory[FT] {
    def getSelfAsReifiedScalaValue: finalTypes.ReifiedScalaValue[T]
    val ofHostType: OfHostType[T]
    val value: T

    def toScala: String =
      if (value.isInstanceOf[String]) {
        s""""$value""""
      } else {
        value.toString
      }

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): ReifiedScalaValue[FT, T] =
      this
  }

  trait ArgumentExpression[FT <: FinalTypes] extends Expression[FT] with any.ArgumentExpression[FT] with Factory[FT] {
    def toScala: String = s"${parameterName.toScala}"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.ArgumentExpression[FT] =
      this
  }

  trait MemberAccessExpression[FT <: FinalTypes] extends Expression[FT] with oo.MemberAccessExpression[FT] with Factory[FT] {
    def toScala: String = s"${owner.toScala}.${field.toScala}"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.MemberAccessExpression[FT] =
      copy(owner = owner.prefixRootPackage(rootPackageName, excludedTypeNames))
  }

  trait SelfReferenceExpression[FT <: FinalTypes] extends Expression[FT] with oo.SelfReferenceExpression[FT] with Factory[FT] {
    def toScala: String = s"this"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.SelfReferenceExpression[FT] =
      this
  }

  trait ObjectInstantiationExpression[FT <: FinalTypes] extends Expression[FT] with oo.ObjectInstantiationExpression[FT] with Factory[FT] {
    def toScala: String = {
      val bodyScala = body.map(_.classBodyDefinitionToScala).getOrElse("")
      s"""new ${tpe.toScala}(${constructorArguments.map(_.toScala).mkString(",")})${bodyScala}"""
    }

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.ObjectInstantiationExpression[FT] =
      copy(
        tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
        constructorArguments = constructorArguments.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        body = body.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
      )
  }

  trait CastExpression[FT <: FinalTypes] extends Expression[FT] with oo.CastExpression[FT] with Factory[FT] {
    def toScala: String = s"${this.expression.toScala}.asInstanceOf[${tpe.toScala}]"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.CastExpression[FT] =
      copy(
        tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
        expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
  }

  trait InstanceOfExpression[FT <: FinalTypes] extends Expression[FT] with oo.InstanceOfExpression[FT] with Factory[FT] {
    def toScala: String = {
      s"${this.expression.toScala}.isInstanceOf[${this.tpe.toScala}]"
    }

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.InstanceOfExpression[FT] =
      copy(
        tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
        expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
  }

  trait SuperReferenceExpression[FT <: FinalTypes] extends Expression[FT] with oo.SuperReferenceExpression[FT] with Factory[FT] {
    def toScala: String = {
      s"super[${parentType.toScala}]"
    }

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.SuperReferenceExpression[FT] =
      copy(
        parentType = parentType.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
  }

  trait BinaryExpression[FT <: FinalTypes] extends Expression[FT] with operatorExpression.BinaryExpression[FT] with Factory[FT] {
    def toScala: String = operator.toScala(left, right)

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): operatorExpression.BinaryExpression[FT] =
      copy(
        left = left.prefixRootPackage(rootPackageName, excludedTypeNames),
        right = right.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
  }

  trait UnaryExpression[FT <: FinalTypes] extends Expression[FT] with operatorExpression.UnaryExpression[FT] with Factory[FT] {
    def toScala: String = operator.toScala(operand)

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): operatorExpression.UnaryExpression[FT] =
      copy(
        operand = operand.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
  }

  trait Operator[FT <: FinalTypes] extends operatorExpression.Operator[FT] with Factory[FT] {
    def toScala(operands: any.Expression[FT]*): String
  }

  trait InfixOperator[FT <: FinalTypes] { self: Factory[FT] =>
    def operator: String
    def toScala(operands: any.Expression[FT]*): String = operands.map(_.toScala).mkString(operator)
  }

  trait PrefixOperator[FT <: FinalTypes] { self: Factory[FT] =>
    def operator: String
    def toScala(operands: any.Expression[FT]*): String = s"(${operator}${operands.head.toScala})"
  }

  trait MathFunctionOperator[FT <: FinalTypes] { self: Factory[FT] =>
    def operator: String
    def toScala(operands: any.Expression[FT]*): String = {
      s"Math.${operator}${operands.map(_.toScala).mkString("(", ", ", ")")}"
    }
  }

  trait PostfixOperator[FT <: FinalTypes] { self: Factory[FT] =>
    def operator: String
    def toScala(operands: any.Expression[FT]*): String = s"(${operands.head.toScala}${operator})"
  }

  trait AddOp[FT <: FinalTypes] extends arithmetic.AddOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "+"
  }

  trait SubOp[FT <: FinalTypes] extends arithmetic.SubOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "-"
  }

  trait MultOp[FT <: FinalTypes] extends arithmetic.MultOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "*"
  }

  trait DivOp[FT <: FinalTypes] extends arithmetic.DivOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "/"
  }

  trait ModOp[FT <: FinalTypes] extends arithmetic.ModOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "%"
  }

  trait LtOp[FT <: FinalTypes] extends arithmetic.LtOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "<"
  }

  trait LeOp[FT <: FinalTypes] extends arithmetic.LeOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "<="
  }

  trait AndOp[FT <: FinalTypes] extends boolean.AndOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "&&"
  }

  trait OrOp[FT <: FinalTypes] extends boolean.OrOp[FT] with Operator[FT] with Factory[FT] with InfixOperator[FT] {
    override def operator: String = "||"
  }

  trait NotOp[FT <: FinalTypes] extends boolean.NotOp[FT] with Operator[FT] with Factory[FT] with PrefixOperator[FT] {
    override def operator: String = "!"
  }

  trait SqrtOp[FT <: FinalTypes] extends realArithmetic.SqrtOp[FT] with Operator[FT] with Factory[FT] with MathFunctionOperator[FT] {
    override def operator: String = "sqrt"
  }

  trait PowOp[FT <: FinalTypes] extends realArithmetic.PowOp[FT] with Operator[FT] with Factory[FT] with MathFunctionOperator[FT] {
    override def operator: String = "pow"
  }

  trait LogOp[FT <: FinalTypes] extends realArithmetic.LogOp[FT] with Operator[FT] with Factory[FT] with MathFunctionOperator[FT] {
    override def operator: String = "log"
  }

  trait SinOp[FT <: FinalTypes] extends realArithmetic.SinOp[FT] with Operator[FT] with Factory[FT] with MathFunctionOperator[FT] {
    override def operator: String = "sin"
  }

  trait CosOp[FT <: FinalTypes] extends realArithmetic.CosOp[FT] with Operator[FT] with Factory[FT] with MathFunctionOperator[FT] {
    override def operator: String = "cos"
  }

  trait AbsOp[FT <: FinalTypes] extends realArithmetic.AbsOp[FT] with Operator[FT] with Factory[FT] with MathFunctionOperator[FT] {
    override def operator: String = "abs"
  }

  trait FloorOp[FT <: FinalTypes] extends realArithmetic.FloorOp[FT] with Operator[FT] with Factory[FT] with MathFunctionOperator[FT] {
    override def operator: String = "floor"
  }

  trait EulersNumber[FT <: FinalTypes] extends realArithmetic.EulersNumber[FT] with Expression[FT] with Factory[FT] {
    override def toScala: String = "Math.E"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): realArithmetic.EulersNumber[FT] =
      this
  }

  trait Pi[FT <: FinalTypes] extends realArithmetic.Pi[FT] with Expression[FT] with Factory[FT] {
    override def toScala: String = "Math.PI"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): realArithmetic.Pi[FT] =
      this
  }


  trait True[FT <: FinalTypes] extends boolean.True[FT] with Expression[FT] with Factory[FT] {
    override def toScala: String = "true"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): boolean.True[FT] =
      this
  }

  trait False[FT <: FinalTypes] extends boolean.False[FT] with Expression[FT] with Factory[FT] {
    override def toScala: String = "false"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): boolean.False[FT] =
      this
  }

  trait Equals[FT <: FinalTypes] extends eqls.Equals[FT] with Expression[FT] with Factory[FT] {
    def toScala: String = s"${left.toScala} == ${right.toScala}"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): eqls.Equals[FT] =
      copy(
        tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
        left = left.prefixRootPackage(rootPackageName, excludedTypeNames),
        right = right.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
  }

  trait Return[FT <: FinalTypes] extends any.Return[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = s"return ${this.expression.toScala}"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.Return[FT] =
      copy(
        expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
  }

  trait LiftExpression[FT <: FinalTypes] extends imperative.LiftExpression[FT] with Statement[FT] with Factory[FT] {
    def toScala: String = s"${this.expression.toScala};"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): imperative.LiftExpression[FT] =
      copy(
        expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
  }

  trait Type[FT <: FinalTypes] extends any.Type[FT] with Factory[FT] {
    def toScala: String
    def toImport: Seq[any.Import[FT]]

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.Type[FT]
  }

  trait ClassReferenceType[FT <: FinalTypes] extends oo.ClassReferenceType[FT] with Type[FT] with Factory[FT] {
    def toScala: String = qualifiedClassName.map(_.toScala).mkString(".")

    def toImport: Seq[any.Import[FT]] = Seq.empty

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.ClassReferenceType[FT] = {
      if (excludedTypeNames.contains(qualifiedClassName)) {
        this
      } else {
        copy(
          qualifiedClassName = rootPackageName ++ qualifiedClassName
        )
      }
    }
  }

  trait Name[FT <: FinalTypes] extends any.Name[FT] with Factory[FT] {
    def component: String
    def mangled: String
    def toScala: String = mangled
  }

  trait Util[FT <: FinalTypes] extends Factory[FT] {
    def nameProvider: NameProvider[any.Name[FT]]
    def reify[T](tpe: OfHostType[T], value: T): any.Expression[FT] = reifiedScalaValue(tpe, value)

    def resolveImport(tpe: any.Type[FT]): Seq[any.Import[FT]] = tpe.toImport
    def getFreshName(basedOn: any.Name[FT]): any.Name[FT] = {
      val id = UUID.randomUUID().toString.replace("-", "")
      nameProvider.mangle(s"${basedOn.component}_${id}")
    }
  }

  trait Method[FT <: FinalTypes] extends generics.Method[FT] with Factory[FT] with Util[FT] {

    def findClass(qualifiedName: any.Name[FT]*): any.Type[FT] =
      classReferenceType(qualifiedName:_*)

    def toScala: String = {
      val overrideMod = if (isOverride) "override" else ""
      val privateMod = if (!isPublic) "private" else ""
      val mods = Seq(overrideMod, privateMod).mkString(" ")

      val params = parameters.map(p => s"${p._1.toScala} : ${p._2.toScala}").mkString(",")
      val typeParams = if (typeParameters.isEmpty) "" else typeParameters.map(_.toScala).mkString("[", ",", "]")
      val returnTpe = returnType.map(_.toScala).getOrElse("Unit")
      val body = if (!isAbstract) {
        s"""= {
           |  ${imports.map(_.toScala).mkString("\n    ")}
           |  ${statements.map(_.toScala).mkString("\n    ")}
           |}
          """.stripMargin
      } else ""

      s"""${mods} def ${name.toScala}${typeParams}(${params}): ${returnTpe} ${body}""".stripMargin
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.Method[FT] = {
      copyAsGenericMethod(
        name = name,
        imports = imports.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        statements = statements.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        returnType = returnType.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        typeParameters = typeParameters.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        parameters = parameters.map { case (name, tpe) => (name, tpe.prefixRootPackage(rootPackageName, excludedTypeNames)) },
        typeLookupMap = tpeRep => typeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
      )
    }
  }

  trait Constructor[FT <: FinalTypes] extends oo.Constructor[FT] with scala.Method[FT] with Factory[FT] {

    override def typeParameters: Seq[polymorphism.TypeParameter[FT]] = Seq.empty
    override def name: any.Name[FT] = nameProvider.mangle(constructedType.map(_.toScala).getOrElse(""))
    override def returnType: Option[any.Type[FT]] = constructedType

    def importDecls: String = imports.map(_.toScala).mkString("\n    ")

    override def toScala: String = {
      val params = parameters.map(p => s"${p._1.toScala} : ${p._2.toScala}").mkString(",")
      val superInitDecls = if (superInitialization.isDefined) {
        s"// Potential Generator Defect: Scala cannot declare supers in secondary constructor"
      } else ""
      val fieldInitDecls = fieldInitializers.map(fi =>{
         s"${fi._2.toScala}"
      }).mkString(", ")
      val bodyDecls =
        s"""
        |  ${statements.map(_.toScala).mkString("\n    ")}
        """.stripMargin

      s"""def this(${params}) = {
         |  this(${fieldInitDecls})
         |  ${importDecls}
         |  ${bodyDecls}
         |}""".stripMargin
    }

    def primaryConstructorParams: String = {
      parameters.map(p => s"${p._1.toScala} : ${p._2.toScala}").mkString("(", ",", ")")
    }

    def parentInitializers(ofClass: oo.Class[FT]): String = {
      val supers = (ofClass.parents ++ ofClass.implemented)
      superInitialization match {
        case Some((parent, parentArgs)) =>
          val rest = supers.filter(sp => sp != parent).map(_.toScala)
          val withRest = if (rest.isEmpty) "" else rest.mkString("with ", " with ", "")
          s"extends ${parent.toScala}(${parentArgs.map(_.toScala).mkString(", ")}) ${withRest}"
        case None =>
          if (supers.nonEmpty) supers.map(_.toScala).mkString("extends ", " with ", "") else ""
      }
    }

    def primaryConstructorBody(ofClass: oo.Class[FT]): String = {
      val (fieldDeclarations, remainingPrimaryInits) = {
        ofClass.fields.foldLeft[(Seq[String], Seq[(any.Name[FT], any.Expression[FT])])]((Seq.empty, fieldInitializers)) {
          case ((decls, remainingInitializers), declaredField) => {
            if (declaredField.init.isDefined) {
              (decls :+ convert(declaredField).toScala, remainingInitializers)
            } else {
              def extractFirstDeclaration(decls: Seq[(any.Name[FT], any.Expression[FT])]): (Seq[(any.Name[FT], any.Expression[FT])], Option[any.Expression[FT]]) =
                decls match {
                  case (name, init) +: rest if name == declaredField.name =>
                    (rest, Some(init))
                  case hd +: rest =>
                    val (remaining, init) = extractFirstDeclaration(rest)
                    (hd +: remaining, init)
                  case _ => (Seq.empty, None)
                }
              val (remaining, init) = extractFirstDeclaration(remainingInitializers)
              (decls :+ convert(declaredField.copy(init = init)).toScala, remaining)
            }
          }
        }
      }
      val overrideInits = remainingPrimaryInits.map(fi => {
        s"this.${fi._1.toScala} = ${fi._2.toScala}"
      }).mkString("  \n")
      val bodyDecls =
        s"""
           |  ${statements.map(_.toScala).mkString("\n    ")}
        """.stripMargin
      s"""
         |  ${importDecls}
         |  ${fieldDeclarations.mkString("\n  ")}
         |  ${overrideInits}
         |  ${bodyDecls}
         |  """.stripMargin
    }

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.Constructor[FT] = {
      copyAsConstructor(
        constructedType = constructedType.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        imports = imports.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        statements = statements.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        parameters = parameters.map { case (name, tpe) => (name, tpe.prefixRootPackage(rootPackageName, excludedTypeNames)) },
        typeLookupMap = tpeRep => typeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        constructorTypeLookupMap = tpeRep => constructorTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        superInitialization = superInitialization.map { case (tpe, exps) =>
          (tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            exps.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)))
        },
        fieldInitializers = fieldInitializers.map { case (name, exp) => (name, exp.prefixRootPackage(rootPackageName, excludedTypeNames)) }
      )
    }
  }

  trait Field[FT <: FinalTypes] extends oo.Field[FT] with Factory[FT] {
    def toScala: String = {
      val initExp = init.map(exp => s" = ${exp.toScala}").getOrElse(s" = null.asInstanceOf[${tpe.toScala}]")
      s"var ${name.toScala}: ${tpe.toScala}${initExp}"
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.Field[FT] = {
      copy(
        tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
        init = init.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
      )
    }

  }

  trait TypeParameter[FT <: FinalTypes] extends generics.TypeParameter[FT] with Factory[FT] {
    def toScala: String = {
      val lbs =
        if (lowerBounds.nonEmpty) lowerBounds.map(_.toScala).mkString(" with ") + " >: " else ""
      val ubs =
        if (upperBounds.nonEmpty) " <: " + upperBounds.map(_.toScala).mkString(" with ") else ""
      s"""${name.toScala}${lbs}${ubs}"""
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): polymorphism.TypeParameter[FT] = {
      copyAsTypeParameterWithBounds(
        upperBounds = upperBounds.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        lowerBounds = lowerBounds.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
      )
    }
  }

  trait TypeReferenceExpression[FT <: FinalTypes] extends polymorphism.TypeReferenceExpression[FT] with Expression[FT] with Factory[FT] {
    def toScala: String = tpe.toScala

    override def isTypeReferenceExpression: Boolean = true

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): polymorphism.TypeReferenceExpression[FT] = {
      copy(
        tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames)
      )
    }
  }

  trait TypeArgument[FT <: FinalTypes] extends polymorphism.TypeArgument[FT] with Type[FT] with Factory[FT] {
    def toScala: String = name.toScala

    def toImport: Seq[any.Import[FT]] = Seq.empty

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): polymorphism.TypeArgument[FT] = {
      this
    }
  }

  trait TypeApplication[FT <: FinalTypes] extends polymorphism.TypeApplication[FT] with Type[FT] with Factory[FT] {
    def toScala: String = {
      s"${function.toScala}[${arguments.map(_.toScala).mkString(", ")}]"
    }

    def toImport: Seq[any.Import[FT]] = function.toImport ++ arguments.flatMap(_.toImport)

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): polymorphism.TypeApplication[FT] = {
      copy(
        function = function.prefixRootPackage(rootPackageName, excludedTypeNames),
        arguments = arguments.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
      )
    }
  }



  trait Class[FT <: FinalTypes] extends generics.Class[FT] with Factory[FT] with Util[FT] {


    def findClass(qualifiedName: any.Name[FT]*): any.Type[FT] = {
      classReferenceType(qualifiedName: _*)
    }

    def classBodyDefinitionToScala: String = {
      val importDecls = imports.map(_.toScala).mkString("\n    ")
      val fieldDecls = fields.map(_.toScala).mkString("\n  ")
      val methodDecls = methods.map(_.toScala).mkString("\n  ")
      val secondaryConstructorDecls = if (constructors.isEmpty) "" else constructors.tail.map(_.toScala).mkString("\n  ")
      val initBlock = constructors.headOption.map(ctor =>
        convert(ctor).primaryConstructorBody(this)
      ).getOrElse(fieldDecls)
      s"""
         |{
         |  ${importDecls}
         |  ${initBlock}
         |  ${secondaryConstructorDecls}
         |  ${methodDecls}
         |}""".stripMargin
    }

    def toScala: String = {
      val kind = if (isInterface) "trait" else if (isStatic) "object" else "class"
      val abstractMod = if (isAbstract && !isInterface) "abstract" else ""
      val typeParams = {
        val ps = typeParameters.map(_.toScala)
        if (!isStatic && ps.nonEmpty) ps.mkString("[", ",", "]") else ""
      }
      val extendsClause = constructors.headOption.map(ctor =>
          convert(ctor).parentInitializers(this)
        ).getOrElse {
          val supers = (parents ++ implemented)
          if (supers.nonEmpty) supers.map(_.toScala).mkString("extends ", " with ", "") else ""
        }
      val primaryConstructorParams = constructors.headOption.map(ctor =>
          convert(ctor).primaryConstructorParams
        ).getOrElse("")


      s"""
         |${abstractMod} ${kind} ${name.toScala}${typeParams}${primaryConstructorParams} ${extendsClause} ${classBodyDefinitionToScala}""".stripMargin
    }
    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): oo.Class[FT] = {
      copyAsGenericClass(
        imports = imports.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        parents = parents.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        implemented = implemented.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        fields = fields.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        methods = methods.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        constructors = constructors.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        methodTypeLookupMap = tpeRep => methodTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        constructorTypeLookupMap = tpeRep => constructorTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        typeLookupMap = tpeRep => typeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
      )
    }
  }

  trait ApplyExpression[FT <: FinalTypes] extends Expression[FT] with any.ApplyExpression[FT] with Factory[FT] {
    def toScala : String = {
      val (typeArguments, regularArguments) = arguments.partition(_.isTypeReferenceExpression)
      val tyArgs = if (typeArguments.isEmpty) "" else typeArguments.map(_.toScala).mkString("[",  ", ", "]")
      val args = if (regularArguments.isEmpty) "" else regularArguments.map(_.toScala).mkString("(",  ", ", ")")
      s"${function.toScala}${tyArgs}${args}"
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.ApplyExpression[FT] = {
      copy(
        function = function.prefixRootPackage(rootPackageName, excludedTypeNames),
        arguments = arguments.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
      )
    }
  }

  trait VariableReferenceExpression[FT <: FinalTypes] extends Expression[FT] with imperative.VariableReferenceExpression[FT] with Factory[FT] {
    def toScala: String = s"${name.toScala}"

    override def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): imperative.VariableReferenceExpression[FT] =
      this
  }

  trait CompilationUnit[FT <: FinalTypes] extends oo.CompilationUnit[FT] with Factory[FT] with Util[FT] {
    def toScala: String = {
      val importDecls = imports.map(_.toScala).mkString("\n    ")
      val clsDecls = classes.map(_.toScala).mkString("\n\n")
      val packageDecl = if (name.init.isEmpty) "" else s"package ${name.init.map(_.toScala).mkString(".")}"
      s"""
         |${packageDecl}
         |${importDecls}
         |${clsDecls}
         |""".stripMargin
    }

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): any.CompilationUnit[FT] = {
      copyAsCompilationUnitWithClasses(
        name = rootPackageName ++ name,
        imports = imports.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        methodTypeLookupMap = tpeRep => methodTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        constructorTypeLookupMap = tpeRep => constructorTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        classTypeLookupMap = tpeRep => classTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
        classes = classes.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
      )
    }
  }

  trait ToStringOp[FT <: FinalTypes] extends strings.ToStringOp[FT] with Operator[FT] with PostfixOperator[FT] {
    def operator: String = ".toString()"
  }

  trait AppendStringOp[FT <: FinalTypes] extends strings.AppendStringOp[FT] with Operator[FT] with InfixOperator[FT] {
    def operator: String = "++"
  }
  trait StringLengthOp[FT <: FinalTypes] extends strings.StringLengthOp[FT] with Operator[FT] with PostfixOperator[FT] {
    def operator: String = ".length"
  }
  trait AssertTrueOp[FT <: FinalTypes] extends assertions.AssertTrueOp[FT] with Operator[FT] with PrefixOperator[FT] {
    def operator: String = "assert "
  }

  trait CreateList[FT <: FinalTypes] extends lists.CreateList[FT] with Type[FT] {
    def toScala: String = "Seq"

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): lists.CreateList[FT] =
      this
    def toImport: Seq[any.Import[FT]] = Seq.empty
  }

  trait CreateLeaf[FT <: FinalTypes] extends trees.CreateLeaf[FT] with Type[FT] {
    def leafClass: oo.ClassReferenceType[FT]
    def toScala: String = leafClass.toScala

    def copyWithLeafClass(leafClass: oo.ClassReferenceType[FT] = leafClass): trees.CreateLeaf[FT] =
      createLeafWithLeafClass(leafClass)

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): trees.CreateLeaf[FT] =
      copyWithLeafClass(leafClass = leafClass.prefixRootPackage(rootPackageName, excludedTypeNames))

    def toImport: Seq[any.Import[FT]] = leafClass.toImport
  }

  trait CreateNodeExpr[FT <: FinalTypes] extends trees.CreateNodeExpr[FT] with Expression[FT] {
    def nodeClass: oo.ClassReferenceType[FT]
    def toScala: String = nodeClass.toScala

    def copyWithNodeClass(nodeClass: oo.ClassReferenceType[FT] = nodeClass): trees.CreateNodeExpr[FT] =
      createNodeExprWithNodeClass(nodeClass)

    def prefixRootPackage(rootPackageName: Seq[any.Name[FT]], excludedTypeNames: Set[Seq[any.Name[FT]]]): trees.CreateNodeExpr[FT] =
      copyWithNodeClass(nodeClass = nodeClass.prefixRootPackage(rootPackageName, excludedTypeNames))
  }

  trait ConsListOp[FT <: FinalTypes] extends lists.ConsListOp[FT] with Operator[FT] with InfixOperator[FT] {
    def operator: String = "+:"
  }

  trait HeadListOp[FT <: FinalTypes] extends lists.HeadListOp[FT] with Operator[FT] with PostfixOperator[FT] {
    def operator: String = ".head"
  }

  trait TailListOp[FT <: FinalTypes] extends lists.TailListOp[FT] with Operator[FT] with PostfixOperator[FT] {
    def operator: String = ".tail"
  }

  trait AppendListOp[FT <: FinalTypes] extends lists.AppendListOp[FT] with Operator[FT] with InfixOperator[FT] {
    def operator: String = "++"
  }

  trait Factory[FT <: FinalTypes]
    extends any.Factory[FT]
    with oo.Factory[FT]
    with imperative.Factory[FT]
    with arithmetic.Factory[FT]
    with realArithmetic.Factory[FT]
    with assertions.Factory[FT]
    with boolean.Factory[FT]
    with eqls.Factory[FT]
    with operatorExpression.Factory[FT]
    with strings.Factory[FT]
    with lists.Factory[FT]
    with trees.Factory[FT]
    with generics.Factory[FT] {

    def name(name: String, mangled: String): Name[FT]
    def importStatement(components: Seq[any.Name[FT]]): Import[FT]

    def reifiedScalaValue[T](ofHostType: OfHostType[T], value: T): ReifiedScalaValue[FT, T]


    override def createNodeExpr(): trees.CreateNodeExpr[FT] = {
      createNodeExprWithNodeClass(classReferenceType(
        Seq("org", "combinators", "ep", "util", "Node").map(n => name(n, n)):_*
      ))
    }
    def createNodeExprWithNodeClass(nodeClass: oo.ClassReferenceType[FT]): CreateNodeExpr[FT]

    override def createLeaf(): trees.CreateLeaf[FT] = {
      createLeafWithLeafClass(classReferenceType(
        Seq("org", "combinators", "ep", "util", "Leaf").map(n => name(n, n)):_*
      ))
    }
    def createLeafWithLeafClass(leafClass: oo.ClassReferenceType[FT]): CreateLeaf[FT]

    implicit def convert(other: any.Import[FT]): Import[FT]
    implicit def convert(other: any.Statement[FT]): Statement[FT]
    implicit def convert(other: any.Type[FT]): Type[FT]
    implicit def convert(other: any.Name[FT]): Name[FT]
    implicit def convert(other: any.Expression[FT]): Expression[FT]
    implicit def convert(other: any.ArgumentExpression[FT]): ArgumentExpression[FT]
    implicit def convert(other: any.Project[FT]): Project[FT]
    implicit def convert(other: any.CompilationUnit[FT]): CompilationUnit[FT]
    implicit def convert(other: any.Method[FT]): Method[FT]
    implicit def convert(other: oo.Class[FT]): Class[FT]
    implicit def convert(other: oo.Constructor[FT]): Constructor[FT]
    implicit def convert(other: oo.Field[FT]): Field[FT]
    implicit def convert(other: oo.MemberAccessExpression[FT]): MemberAccessExpression[FT]
    implicit def convert(other: oo.SelfReferenceExpression[FT]): SelfReferenceExpression[FT]
    implicit def convert(other: oo.ObjectInstantiationExpression[FT]): ObjectInstantiationExpression[FT]
    implicit def convert(other: oo.CastExpression[FT]): CastExpression[FT]
    implicit def convert(other: oo.InstanceOfExpression[FT]): InstanceOfExpression[FT]
    implicit def convert(other: oo.SuperReferenceExpression[FT]): SuperReferenceExpression[FT]
    implicit def convert(decl: imperative.DeclareVariable[FT]): DeclareVariable[FT]
    implicit def convert(assignVariable: imperative.AssignVariable[FT]): AssignVariable[FT]
    implicit def convert(ifThenElse: imperative.IfThenElse[FT]): IfThenElse[FT]
    implicit def convert(whileLoop: imperative.While[FT]): While[FT]
    implicit def convert(operator: operatorExpression.Operator[FT]): Operator[FT]
    implicit def convert(binaryExpression: operatorExpression.BinaryExpression[FT]): BinaryExpression[FT]
    implicit def convert(unaryExpression: operatorExpression.UnaryExpression[FT]): UnaryExpression[FT]

    implicit def convert[T](reifiedScalaValue: ReifiedScalaValue[FT, T]): ReifiedScalaValue[FT, T]
    implicit def convert(other: any.ApplyExpression[FT]): ApplyExpression[FT]
    implicit def convert(other: oo.ClassReferenceType[FT]): ClassReferenceType[FT]
    implicit def convert(varRef: imperative.VariableReferenceExpression[FT]): VariableReferenceExpression[FT]

    override implicit def convert(other: polymorphism.TypeParameter[FT]): TypeParameter[FT]
    override implicit def convert(other: polymorphism.TypeReferenceExpression[FT]): TypeReferenceExpression[FT]
    override implicit def convert(other: polymorphism.TypeArgument[FT]): TypeArgument[FT]
    override implicit def convert(other: polymorphism.TypeApplication[FT]): TypeApplication[FT]

    implicit def convert(other: trees.CreateLeaf[FT]): CreateLeaf[FT]
    implicit def convert(other: trees.CreateNodeExpr[FT]): CreateNodeExpr[FT]


  }

  object Finalized {
    class FinalTypes extends scala.FinalTypes {
      override type DeclareVariable = Finalized.DeclareVariable
      override type AssignVariable = Finalized.AssignVariable
      override type IfThenElse = Finalized.IfThenElse
      override type While = Finalized.While
      override type Class = Finalized.Class
      override type Constructor = Finalized.Constructor
      override type Field = Finalized.Field
      override type MemberAccessExpression = Finalized.MemberAccessExpression
      override type SelfReferenceExpression = Finalized.SelfReferenceExpression
      override type ObjectInstantiationExpression = Finalized.ObjectInstantiationExpression
      override type CastExpression = Finalized.CastExpression
      override type InstanceOfExpression = Finalized.InstanceOfExpression
      override type SuperReferenceExpression = Finalized.SuperReferenceExpression
      override type Operator = Finalized.Operator
      override type BinaryExpression = Finalized.BinaryExpression
      override type UnaryExpression = Finalized.UnaryExpression
      override type Import = Finalized.Import
      override type Statement = Finalized.Statement
      override type Type = Finalized.Type
      override type Name = Finalized.Name
      override type Expression = Finalized.Expression
      override type ApplyExpression = Finalized.ApplyExpression
      override type ArgumentExpression = Finalized.ArgumentExpression
      override type CompilationUnit = Finalized.CompilationUnit
      override type Project = Finalized.Project
      override type Method = scala.Method[FinalTypes]
      override type ReifiedScalaValue[T] = Finalized.ReifiedScalaValue[T]
      override type ClassReferenceType = Finalized.ClassReferenceType
      override type VariableReferenceExpression = Finalized.VariableReferenceExpression
      override type TypeParameter = Finalized.TypeParameter
      override type TypeReferenceExpression = Finalized.TypeReferenceExpression
      override type TypeArgument = Finalized.TypeArgument
      override type TypeApplication = Finalized.TypeApplication
      override type CreateLeaf = Finalized.CreateLeaf
      override type CreateNodeExpr = Finalized.CreateNodeExpr
    }

    trait Factory extends scala.Factory[FinalTypes] {
      val finalTypes: FinalTypes = new FinalTypes

      def name(name: String, mangled: String): Name = Name(name, mangled)
      override def importStatement(components: Seq[any.Name[FinalTypes]]): Import = Import(components)
      override def binaryExpression(operator: operatorExpression.Operator[FinalTypes], left: any.Expression[FinalTypes], right: any.Expression[FinalTypes]): operatorExpression.BinaryExpression[FinalTypes] = BinaryExpression(operator, left, right)
      override def unaryExpression(operator: operatorExpression.Operator[FinalTypes], operand: any.Expression[FinalTypes]): operatorExpression.UnaryExpression[FinalTypes] = UnaryExpression(operator, operand)
      implicit def convert(operator: operatorExpression.Operator[FinalTypes]): Operator = operator.getSelfOperator
      implicit def convert(binaryExpression: operatorExpression.BinaryExpression[FinalTypes]): BinaryExpression = binaryExpression.getSelfBinaryExpression
      implicit def convert(unaryExpression: operatorExpression.UnaryExpression[FinalTypes]): UnaryExpression = unaryExpression.getSelfUnaryExpression
      override def compilationUnit(name: Seq[any.Name[FinalTypes]],
        imports: Seq[any.Import[FinalTypes]],
        methodTypeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]] = Map.empty,
        constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FinalTypes], any.Type[FinalTypes]] = Map.empty,
        classTypeLookupMap: TypeRep => Generator[oo.Class[FinalTypes], any.Type[FinalTypes]] = Map.empty,
        classes: Seq[oo.Class[FinalTypes]]): oo.CompilationUnit[FinalTypes] = CompilationUnit(name, imports, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap, classes)


      override def constructor(
        constructedType: Option[any.Type[FinalTypes]],
        imports: Set[any.Import[FinalTypes]],
        statements: Seq[any.Statement[FinalTypes]],
        parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
        typeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]],
        constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FinalTypes], any.Type[FinalTypes]],
        superInitialization: Option[(any.Type[FinalTypes], Seq[any.Expression[FinalTypes]])],
        fieldInitializers: Seq[(any.Name[FinalTypes], any.Expression[FinalTypes])]): Constructor =
        Constructor(
          constructedType = constructedType,
          imports = imports,
          statements = statements,
          parameters = parameters,
          typeLookupMap = typeLookupMap,
          constructorTypeLookupMap = constructorTypeLookupMap,
          superInitialization = superInitialization,
          fieldInitializers = fieldInitializers
        )

      override def field(name: any.Name[FinalTypes], tpe: any.Type[FinalTypes], init: Option[any.Expression[FinalTypes]]): oo.Field[FinalTypes] = Field(name, tpe, init)
      override def memberAccessExpression(owner: any.Expression[FinalTypes], field: any.Name[FinalTypes]): MemberAccessExpression = MemberAccessExpression(owner, field)

      override def objectInstantiationExpression(tpe: any.Type[FinalTypes], constructorArguments: Seq[any.Expression[FinalTypes]], body: Option[oo.Class[FinalTypes]] = Option.empty): oo.ObjectInstantiationExpression[FinalTypes] =
        ObjectInstantiationExpression(tpe, constructorArguments, body)
      override def castExpression(tpe: any.Type[FinalTypes], expression: any.Expression[FinalTypes]): oo.CastExpression[FinalTypes] = CastExpression(tpe, expression)
      override def instanceOfExpression(tpe: any.Type[FinalTypes], expression: any.Expression[FinalTypes]): oo.InstanceOfExpression[FinalTypes] = InstanceOfExpression(tpe, expression)
      override def superReferenceExpression(parentType: any.Type[FinalTypes]): oo.SuperReferenceExpression[FinalTypes] = SuperReferenceExpression(parentType)
      override def selfReferenceExpression: oo.SelfReferenceExpression[FinalTypes] = SelfReferenceExpression()
      implicit def convert(other: any.Project[FinalTypes]): Project = other.getSelfProject
      implicit def convert(other: any.CompilationUnit[FinalTypes]): CompilationUnit = other.getSelfCompilationUnit
      implicit def convert(other: any.Method[FinalTypes]): scala.Method[FinalTypes] = other.getSelfMethod
      implicit def convert(other: oo.Class[FinalTypes]): Class = other.getSelfClass
      implicit def convert(other: oo.Constructor[FinalTypes]): Constructor = other.getSelfConstructor
      implicit def convert(other: oo.Field[FinalTypes]): Field = other.getSelfField
      implicit def convert(other: oo.MemberAccessExpression[FinalTypes]): MemberAccessExpression = other.getSelfMemberAccessExpression
      implicit def convert(other: oo.SelfReferenceExpression[FinalTypes]): SelfReferenceExpression = other.getSelfSelfReferenceExpression
      implicit def convert(other: oo.ObjectInstantiationExpression[FinalTypes]): ObjectInstantiationExpression = other.getSelfObjectInstantiationExpression
      implicit def convert(other: oo.CastExpression[FinalTypes]): CastExpression = other.getSelfCastExpression
      implicit def convert(other: oo.InstanceOfExpression[FinalTypes]): InstanceOfExpression = other.getSelfInstanceOfExpression
      implicit def convert(other: oo.SuperReferenceExpression[FinalTypes]): SuperReferenceExpression = other.getSelfSuperReferenceExpression
      override def addOp(): arithmetic.AddOp[FinalTypes] = AddOp()
      override def subOp(): arithmetic.SubOp[FinalTypes] = SubOp()
      override def multOp(): arithmetic.MultOp[FinalTypes] = MultOp()
      override def divOp(): arithmetic.DivOp[FinalTypes] = DivOp()
      override def modOp(): arithmetic.ModOp[FinalTypes] = ModOp()
      def sqrtOp(): realArithmetic.SqrtOp[FinalTypes] = SqrtOp()
      def powOp(): realArithmetic.PowOp[FinalTypes] = PowOp()
      def logOp(): realArithmetic.LogOp[FinalTypes] = LogOp()
      def sinOp(): realArithmetic.SinOp[FinalTypes] = SinOp()
      def cosOp(): realArithmetic.CosOp[FinalTypes] = CosOp()
      def absOp(): realArithmetic.AbsOp[FinalTypes] = AbsOp()
      def floorOp(): realArithmetic.FloorOp[FinalTypes] = FloorOp()
      def pi(): realArithmetic.Pi[FinalTypes] = Pi()
      def eulersNumber(): realArithmetic.EulersNumber[FinalTypes] = EulersNumber()
      override def ltOp(): arithmetic.LtOp[FinalTypes] = LtOp()
      override def leOp(): arithmetic.LeOp[FinalTypes] = LeOp()
      override def equals(tpe: any.Type[FinalTypes], left: any.Expression[FinalTypes], right: any.Expression[FinalTypes]): eqls.Equals[FinalTypes] = Equals(tpe, left, right)
      override def andOp(): boolean.AndOp[FinalTypes] = AndOp()
      override def orOp(): boolean.OrOp[FinalTypes] = OrOp()
      override def notOp(): boolean.NotOp[FinalTypes] = NotOp()
      override def trueExp(): boolean.True[FinalTypes] = True()
      override def falseExp(): boolean.False[FinalTypes] = False()
      override def declareVariable(name: any.Name[FinalTypes], tpe: any.Type[FinalTypes], initializer: Option[any.Expression[FinalTypes]]): imperative.DeclareVariable[FinalTypes] =
        DeclareVariable(name, tpe, initializer)
      override def assignVariable(variable: any.Expression[FinalTypes], expression: any.Expression[FinalTypes]): imperative.AssignVariable[FinalTypes] =
        AssignVariable(variable, expression)
      override def liftExpression(expression: any.Expression[FinalTypes]): imperative.LiftExpression[FinalTypes] =
        LiftExpression(expression)
      override def returnExpression(expression: any.Expression[FinalTypes]): any.Return[FinalTypes] =
        Return(expression)
      override def ifThenElse(condition: any.Expression[FinalTypes], ifBranch: Seq[any.Statement[FinalTypes]], elseIfBranches: Seq[(any.Expression[FinalTypes], Seq[any.Statement[FinalTypes]])], elseBranch: Seq[any.Statement[FinalTypes]]): imperative.IfThenElse[FinalTypes] =
        IfThenElse(condition, ifBranch, elseIfBranches, elseBranch)
      override def whileLoop(condition: any.Expression[FinalTypes], body: Seq[any.Statement[FinalTypes]]): imperative.While[FinalTypes] =
        While(condition, body)
      implicit def convert(decl: imperative.DeclareVariable[FinalTypes]): DeclareVariable = decl.getSelfDeclareVariable
      implicit def convert(assignVariable: imperative.AssignVariable[FinalTypes]): AssignVariable = assignVariable.getSelfAssignVariable
      implicit def convert(ifThenElse: imperative.IfThenElse[FinalTypes]): IfThenElse = ifThenElse.getSelfIfThenElse
      implicit def convert(whileLoop: imperative.While[FinalTypes]): While = whileLoop.getSelfWhile
      override def toStringOp(): strings.ToStringOp[FinalTypes] = ToStringOp()
      override def appendStringOp(): strings.AppendStringOp[FinalTypes] = AppendStringOp()
      override def stringLengthOp(): strings.StringLengthOp[FinalTypes] = StringLengthOp()
      override def assertTrueOp(): assertions.AssertTrueOp[FinalTypes] = AssertTrueOp()

      override def ooProject(compilationUnits: Set[any.CompilationUnit[FinalTypes]],
        methodTypeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]],
        constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FinalTypes], any.Type[FinalTypes]],
        classTypeLookupMap: TypeRep => Generator[oo.Class[FinalTypes], any.Type[FinalTypes]]): any.Project[FinalTypes] = Project(compilationUnits, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap)
      override def argumentExpression(parameterName: any.Name[FinalTypes]): any.ArgumentExpression[FinalTypes] = ArgumentExpression(parameterName)
      implicit def convert(other: any.Import[FinalTypes]): Import = other.getSelfImport
      implicit def convert(other: any.Statement[FinalTypes]): Statement = other.getSelfStatement
      implicit def convert(other: any.Type[FinalTypes]): Type = other.getSelfType
      implicit def convert(other: any.Name[FinalTypes]): Name = other.getSelfName
      implicit def convert(other: any.Expression[FinalTypes]): Expression = other.getSelfExpression
      implicit def convert(other: any.ArgumentExpression[FinalTypes]): ArgumentExpression = other.getSelfArgumentExpression

      override def reifiedScalaValue[T](ofHostType: OfHostType[T], value: T): ReifiedScalaValue[T] = ReifiedScalaValue(ofHostType, value)
      implicit def convert[T](other: scala.ReifiedScalaValue[FinalTypes, T]): ReifiedScalaValue[T] = other.getSelfAsReifiedScalaValue
      implicit def convert(other: any.ApplyExpression[FinalTypes]): ApplyExpression = other.getSelfApplyExpression
      implicit def convert(other: oo.ClassReferenceType[FinalTypes]): ClassReferenceType = other.getSelfClassReferenceType
      implicit def convert(varRef: imperative.VariableReferenceExpression[FinalTypes]): VariableReferenceExpression = varRef.getSelfVariableReferenceExpression
      override def variableReferenceExpression(name: any.Name[FinalTypes]): imperative.VariableReferenceExpression[FinalTypes] = VariableReferenceExpression(name)

      override def classReferenceType(qualifiedClassName: any.Name[FinalTypes]*): oo.ClassReferenceType[FinalTypes] = ClassReferenceType(qualifiedClassName)
      override def applyExpression(function: any.Expression[FinalTypes], arguments: Seq[any.Expression[FinalTypes]]): any.ApplyExpression[FinalTypes] = ApplyExpression(function, arguments)

      override def convert(other: polymorphism.TypeParameter[FinalTypes]): TypeParameter = other.getSelfTypeParameter

      override implicit def convert(other: polymorphism.TypeReferenceExpression[FinalTypes]): TypeReferenceExpression = other.getSelfTypeReferenceExpression

      override implicit def convert(other: polymorphism.TypeArgument[FinalTypes]): TypeArgument = other.getSelfTypeArgument

      override implicit def convert(other: polymorphism.TypeApplication[FinalTypes]): TypeApplication = other.getSelfTypeApplication

      override def genericClass(name: any.Name[FinalTypes],
        imports: Seq[any.Import[FinalTypes]],
        typeParameters: Seq[polymorphism.TypeParameter[FinalTypes]],
        parents: Seq[any.Type[FinalTypes]],
        implemented: Seq[any.Type[FinalTypes]],
        fields: Seq[oo.Field[FinalTypes]],
        methods: Seq[any.Method[FinalTypes]],
        constructors: Seq[oo.Constructor[FinalTypes]],
        methodTypeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]],
        constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FinalTypes], any.Type[FinalTypes]],
        typeLookupMap: TypeRep => Generator[oo.Class[FinalTypes], any.Type[FinalTypes]],
        isAbstract: Boolean,
        isInterface: Boolean,
        isStatic: Boolean): oo.Class[FinalTypes] =
        Class(
          name = name,
          typeParameters = typeParameters,
          imports = imports,
          parents = parents,
          implemented = implemented,
          fields = fields,
          methods = methods,
          constructors = constructors,
          methodTypeLookupMap = methodTypeLookupMap,
          constructorTypeLookupMap = constructorTypeLookupMap,
          typeLookupMap = typeLookupMap,
          isAbstract = isAbstract,
          isInterface = isInterface,
          isStatic = isStatic)

      override def genericMethod(
        name: any.Name[FinalTypes],
        imports: Set[any.Import[FinalTypes]],
        statements: Seq[any.Statement[FinalTypes]],
        returnType: Option[any.Type[FinalTypes]],
        typeParameters: Seq[polymorphism.TypeParameter[FinalTypes]],
        parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
        typeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]],
        isAbstract: Boolean,
        isStatic: Boolean,
        isPublic: Boolean,
        isOverride: Boolean): generics.Method[FinalTypes] =
        Method(
          name = name,
          typeParameters = typeParameters,
          imports = imports,
          statements = statements,
          returnType = returnType,
          parameters = parameters,
          typeLookupMap = typeLookupMap,
          isAbstract = isAbstract,
          isStatic = isStatic,
          isPublic = isPublic,
          isOverride = isOverride
        )

      override def typeParameterWithBounds(name: any.Name[FinalTypes], upperBounds: Seq[any.Type[FinalTypes]], lowerBounds: Seq[any.Type[FinalTypes]]): TypeParameter =
        TypeParameter(name, lowerBounds = lowerBounds, upperBounds = upperBounds)
      override def createList(): lists.CreateList[FinalTypes] = CreateList()

      override def consListOp(): lists.ConsListOp[FinalTypes] = ConsListOp()

      override def headListOp(): lists.HeadListOp[FinalTypes] = HeadListOp()

      override def tailListOp(): lists.TailListOp[FinalTypes] = TailListOp()

      override def appendListOp(): lists.AppendListOp[FinalTypes] = AppendListOp()

      override def typeArgument(name: any.Name[FinalTypes]): TypeArgument =
        TypeArgument(name)

      override def typeApplication(function: any.Type[FinalTypes], arguments: Seq[any.Type[FinalTypes]]): TypeApplication =
        TypeApplication(function, arguments)

      override def typeReferenceExpression(tpe: any.Type[FinalTypes]): TypeReferenceExpression =
        TypeReferenceExpression(tpe)
      def createNodeExprWithNodeClass(nodeClass: oo.ClassReferenceType[FinalTypes]): scala.CreateNodeExpr[FinalTypes] = CreateNodeExpr(nodeClass)
      def createLeafWithLeafClass(leafClass: oo.ClassReferenceType[FinalTypes]): scala.CreateLeaf[FinalTypes] = CreateLeaf(leafClass)
      implicit def convert(other: trees.CreateLeaf[FinalTypes]): scala.CreateLeaf[FinalTypes] = other.getSelfCreateLeaf
      implicit def convert(other: trees.CreateNodeExpr[FinalTypes]): scala.CreateNodeExpr[FinalTypes] = other.getSelfCreateNodeExpr
    }

    case class Name(override val component: String, override val mangled: String) extends scala.Name[FinalTypes] with Factory {
      override def getSelfName: this.type = this
    }

    trait Util extends scala.Util[FinalTypes] with Factory {
      override def nameProvider: NameProvider[any.Name[FinalTypes]] = new ScalaNameProvider[FinalTypes](this)
    }

    case class Method(
      override val name: any.Name[FinalTypes],
      override val typeParameters: Seq[polymorphism.TypeParameter[FinalTypes]],
      override val imports: Set[any.Import[FinalTypes]],
      override val statements: Seq[any.Statement[FinalTypes]],
      override val returnType: Option[any.Type[FinalTypes]],
      override val parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
      override val typeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]],
      override val isAbstract: Boolean,
      override val isStatic: Boolean,
      override val isPublic: Boolean,
      override val isOverride: Boolean
    ) extends scala.Method[FinalTypes] with Util {
      def getSelfMethod: this.type = this
    }

    case class Import(override val components: Seq[any.Name[FinalTypes]]) extends scala.Import[FinalTypes] with Factory {
      def getSelfImport: this.type = this
    }

    trait Type extends scala.Type[FinalTypes] with Factory {
      def getSelfType: this.type = this
    }

    case class ClassReferenceType(override val qualifiedClassName: Seq[any.Name[FinalTypes]]) extends scala.ClassReferenceType[FinalTypes] with Type with Factory {
      def getSelfClassReferenceType: this.type = this
    }

    trait Operator extends scala.Operator[FinalTypes] with Factory {
      override def getSelfOperator: this.type = this
    }

    trait Expression extends scala.Expression[FinalTypes] with Factory {
      override def getSelfExpression: this.type = this
    }

    case class BinaryExpression(
      override val operator: operatorExpression.Operator[FinalTypes],
      override val left: any.Expression[FinalTypes],
      override val right: any.Expression[FinalTypes]
    )
      extends Expression with scala.BinaryExpression[FinalTypes] with Factory {
      override def getSelfBinaryExpression: this.type = this

    }

    case class UnaryExpression(
      override val operator: operatorExpression.Operator[FinalTypes],
      override val operand: any.Expression[FinalTypes]
    )
      extends Expression with scala.UnaryExpression[FinalTypes] with Factory {
      override def getSelfUnaryExpression: this.type = this
    }

    case class ReifiedScalaValue[T](
      override val ofHostType: OfHostType[T],
      override val value: T
    ) extends Expression with scala.ReifiedScalaValue[FinalTypes, T] with Factory {
      override def getSelfAsReifiedScalaValue: this.type = this
    }

    trait Statement extends scala.Statement[FinalTypes] with Factory {
      def getSelfStatement: this.type = this
    }

    case class DeclareVariable(
      override val name: any.Name[FinalTypes],
      override val tpe: any.Type[FinalTypes],
      override val initializer: Option[any.Expression[FinalTypes]]
    ) extends Statement with scala.DeclareVariable[FinalTypes] with Factory {
      override def getSelfDeclareVariable: this.type = this
    }

    case class AssignVariable(
      override val variable: any.Expression[FinalTypes],
      override val assignmentExpression: any.Expression[FinalTypes]
    ) extends Statement with scala.AssignVariable[FinalTypes] with Factory {
      override def getSelfAssignVariable: this.type = this
    }

    case class IfThenElse(
      override val condition: any.Expression[FinalTypes],
      override val ifBranch: Seq[any.Statement[FinalTypes]],
      override val elseIfBranches: Seq[(any.Expression[FinalTypes], Seq[any.Statement[FinalTypes]])],
      override val elseBranch: Seq[any.Statement[FinalTypes]]
    ) extends Statement with scala.IfThenElse[FinalTypes] with Factory {
      override def getSelfIfThenElse: this.type = this
    }

    case class While(
      override val condition: any.Expression[FinalTypes],
      override val body: Seq[any.Statement[FinalTypes]]
    ) extends Statement with scala.While[FinalTypes] with Factory {
      override def getSelfWhile: this.type = this
    }


    case class Class(
      override val name: any.Name[FinalTypes],
      override val typeParameters: Seq[polymorphism.TypeParameter[FinalTypes]],
      override val imports: Seq[any.Import[FinalTypes]] = Seq.empty,
      override val parents: Seq[any.Type[FinalTypes]] = Seq.empty,
      override val implemented: Seq[any.Type[FinalTypes]] = Seq.empty,
      override val fields: Seq[oo.Field[FinalTypes]] = Seq.empty,
      override val methods: Seq[any.Method[FinalTypes]] = Seq.empty,
      override val constructors: Seq[oo.Constructor[FinalTypes]] = Seq.empty,
      override val methodTypeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]],
      override val constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FinalTypes], any.Type[FinalTypes]],
      override val typeLookupMap: TypeRep => Generator[oo.Class[FinalTypes], any.Type[FinalTypes]] = Map.empty,
      override val isAbstract: Boolean = false,
      override val isInterface: Boolean = false,
      override val isStatic: Boolean = false
    ) extends scala.Class[FinalTypes] with Util {
      override def getSelfClass: this.type = this
    }

    case class Constructor(
      override val constructedType: Option[any.Type[FinalTypes]],
      override val imports: Set[any.Import[FinalTypes]],
      override val statements: Seq[any.Statement[FinalTypes]],
      override val parameters: Seq[(any.Name[FinalTypes], any.Type[FinalTypes])],
      override val typeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]],
      override val constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FinalTypes], any.Type[FinalTypes]],
      override val superInitialization: Option[(any.Type[FinalTypes], Seq[any.Expression[FinalTypes]])],
      override val fieldInitializers: Seq[(any.Name[FinalTypes], any.Expression[FinalTypes])]) extends scala.Constructor[FinalTypes] with Util {
      override def getSelfMethod: scala.Method[FinalTypes] = this
      override def getSelfConstructor: this.type = this
      override def toScala: String = {
        super[Constructor].toScala
      }
    }

    case class Field(
      override val name: any.Name[FinalTypes],
      override val tpe: any.Type[FinalTypes],
      override val init: Option[any.Expression[FinalTypes]] = Option.empty
    ) extends scala.Field[FinalTypes] with Factory {
      override def getSelfField: this.type = this
    }

    case class MemberAccessExpression(
      override val owner: any.Expression[FinalTypes],
      override val field: any.Name[FinalTypes]
    ) extends Expression with scala.MemberAccessExpression[FinalTypes] with Factory {
      override def getSelfMemberAccessExpression: this.type = this
    }

    case class SelfReferenceExpression() extends Expression with scala.SelfReferenceExpression[FinalTypes] with Factory {
      override def getSelfSelfReferenceExpression: this.type = this
    }

    case class ObjectInstantiationExpression(
      override val tpe: any.Type[FinalTypes],
      override val constructorArguments: Seq[any.Expression[FinalTypes]],
      override val body: Option[oo.Class[FinalTypes]] = Option.empty
    ) extends Expression with scala.ObjectInstantiationExpression[FinalTypes] with Factory {
      override def getSelfObjectInstantiationExpression: this.type = this
    }

    case class CastExpression(
      override val tpe: any.Type[FinalTypes],
      override val expression: any.Expression[FinalTypes]
    ) extends Expression with scala.CastExpression[FinalTypes] with Factory {
      override def getSelfCastExpression: this.type = this
    }

    case class InstanceOfExpression(
      override val tpe: any.Type[FinalTypes],
      override val expression: any.Expression[FinalTypes]
    ) extends Expression with scala.InstanceOfExpression[FinalTypes] with Factory {
      override def getSelfInstanceOfExpression: this.type = this
    }

    case class SuperReferenceExpression(
      override val parentType: any.Type[FinalTypes]
    ) extends Expression with scala.SuperReferenceExpression[FinalTypes] with Factory {
      override def getSelfSuperReferenceExpression: this.type = this
    }

    case class ArgumentExpression(
      override val parameterName: any.Name[FinalTypes]
    ) extends Expression with scala.ArgumentExpression[FinalTypes] with Factory {
      override def getSelfArgumentExpression: this.type = this
    }

    case class CompilationUnit(
      override val name: Seq[any.Name[FinalTypes]] = Seq.empty,
      override val imports: Seq[any.Import[FinalTypes]] = Seq.empty,
      override val methodTypeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]] = Map.empty,
      override val constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FinalTypes], any.Type[FinalTypes]] = Map.empty,
      override val classTypeLookupMap: TypeRep => Generator[oo.Class[FinalTypes], any.Type[FinalTypes]] = Map.empty,
      override val classes: Seq[oo.Class[FinalTypes]] = Seq.empty
    ) extends scala.CompilationUnit[FinalTypes] with Util {
      override def getSelfCompilationUnit: this.type = this
    }

    case class Project(
      override val compilationUnits: Set[any.CompilationUnit[FinalTypes]],
      override val methodTypeLookupMap: TypeRep => Generator[any.Method[FinalTypes], any.Type[FinalTypes]] = Map.empty,
      override val constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FinalTypes], any.Type[FinalTypes]] = Map.empty,
      override val classTypeLookupMap: TypeRep => Generator[oo.Class[FinalTypes], any.Type[FinalTypes]] = Map.empty
    ) extends scala.Project[FinalTypes] with Factory {
      override def getSelfProject: this.type = this
    }


    case class AddOp() extends scala.AddOp[FinalTypes] with Operator with Factory

    case class SubOp() extends scala.SubOp[FinalTypes] with Operator with Factory

    case class MultOp() extends scala.MultOp[FinalTypes] with Operator with Factory

    case class DivOp() extends scala.DivOp[FinalTypes] with Operator with Factory

    case class ModOp() extends scala.ModOp[FinalTypes] with Operator with Factory

    case class LtOp() extends scala.LtOp[FinalTypes] with Operator with Factory

    case class LeOp() extends scala.LeOp[FinalTypes] with Operator with Factory

    case class SqrtOp() extends scala.SqrtOp[FinalTypes] with Operator with Factory
    case class PowOp() extends scala.PowOp[FinalTypes] with Operator with Factory
    case class LogOp() extends scala.LogOp[FinalTypes] with Operator with Factory
    case class SinOp() extends scala.SinOp[FinalTypes] with Operator with Factory
    case class CosOp() extends scala.CosOp[FinalTypes] with Operator with Factory
    case class AbsOp() extends scala.AbsOp[FinalTypes] with Operator with Factory
    case class FloorOp() extends scala.FloorOp[FinalTypes] with Operator with Factory

    case class EulersNumber() extends scala.EulersNumber[FinalTypes] with Expression with Factory

    case class Pi() extends scala.Pi[FinalTypes] with Expression with Factory

    case class Equals(
      override val tpe: any.Type[FinalTypes],
      override val left: any.Expression[FinalTypes],
      override val right: any.Expression[FinalTypes]
    ) extends scala.Equals[FinalTypes] with Expression with Factory

    case class AndOp() extends scala.AndOp[FinalTypes] with Operator with Factory
    case class OrOp() extends scala.OrOp[FinalTypes] with Operator with Factory
    case class NotOp() extends scala.NotOp[FinalTypes] with Operator with Factory
    case class True() extends scala.True[FinalTypes] with Expression with Factory
    case class False() extends scala.False[FinalTypes] with Expression with Factory

    case class Return(expression: any.Expression[FinalTypes]) extends scala.Return[FinalTypes] with Statement with Factory
    case class LiftExpression(expression: any.Expression[FinalTypes]) extends scala.LiftExpression[FinalTypes] with Statement with Factory

    case class ToStringOp() extends scala.ToStringOp[FinalTypes] with Operator

    case class AppendStringOp() extends scala.AppendStringOp[FinalTypes] with Operator

    case class StringLengthOp() extends scala.StringLengthOp[FinalTypes] with Operator

    case class AssertTrueOp() extends scala.AssertTrueOp[FinalTypes] with Operator

    case class ApplyExpression(
      override val function: any.Expression[FinalTypes],
      override val arguments: Seq[any.Expression[FinalTypes]]
    ) extends scala.ApplyExpression[FinalTypes] with Expression {
      override def getSelfApplyExpression: this.type = this
    }

    case class VariableReferenceExpression(override val name: any.Name[FinalTypes]) extends scala.VariableReferenceExpression[FinalTypes] with Expression with Factory {
      override def getSelfVariableReferenceExpression: this.type = this
    }

    case class TypeParameter(override val name: any.Name[FinalTypes], override val lowerBounds: Seq[any.Type[FinalTypes]], override val upperBounds: Seq[any.Type[FinalTypes]]) extends scala.TypeParameter[FinalTypes] with Factory {
      override def getSelfTypeParameter: this.type = this
    }

    case class TypeArgument(override val name: any.Name[FinalTypes]) extends scala.TypeArgument[FinalTypes] with Type with Factory {
      override def getSelfTypeArgument: this.type = this
    }

    case class TypeReferenceExpression(override val tpe: any.Type[FinalTypes]) extends scala.TypeReferenceExpression[FinalTypes] with Expression with Factory {
      override def getSelfTypeReferenceExpression: this.type = this
    }

    case class TypeApplication(override val function: any.Type[FinalTypes], override val arguments: Seq[any.Type[FinalTypes]]) extends scala.TypeApplication[FinalTypes] with Type with Factory {
      override def getSelfTypeApplication: this.type = this
    }

    case class CreateList() extends scala.CreateList[FinalTypes] with Type {
    }

    case class ConsListOp() extends scala.ConsListOp[FinalTypes] with Operator {
    }

    case class HeadListOp() extends scala.HeadListOp[FinalTypes] with Operator {
    }

    case class TailListOp() extends scala.TailListOp[FinalTypes] with Operator {
    }

    case class AppendListOp() extends scala.AppendListOp[FinalTypes] with Operator {
    }

    case class CreateLeaf(override val leafClass: oo.ClassReferenceType[FinalTypes]) extends scala.CreateLeaf[FinalTypes] with Type {
      def getSelfCreateLeaf: this.type = this
    }

    case class CreateNodeExpr(override val nodeClass: oo.ClassReferenceType[FinalTypes]) extends scala.CreateNodeExpr[FinalTypes] with Expression {
      def getSelfCreateNodeExpr: this.type = this
    }

  }
}
