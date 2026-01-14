package org.combinators.ep.language.scala.ast

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.TypeRep.OfHostType
import org.combinators.cogen.{FileWithPath, NameProvider, TypeRep}
import org.combinators.ep.language.inbetween.functional
import org.combinators.ep.language.inbetween.functional.FunctionalAST
import org.combinators.ep.language.inbetween.functional.control.FunctionalControlAST
import org.combinators.ep.language.inbetween.imperative.ImperativeAST
import org.combinators.ep.language.inbetween.oo.OOAST
import org.combinators.ep.language.inbetween.polymorphism.generics.GenericsAST

import java.util.UUID


trait BaseAST extends OOAST with FunctionalAST with GenericsAST with FunctionalControlAST with ImperativeAST with NameProviderAST {
  object scalaBase {
    object anyOverrides {
      trait FinalTypes extends oo.anyOverrides.FinalTypes 
        with functional.anyOverrides.FinalTypes 
        with funcontrol.anyOverrides.FinalTypes 
        with generics.anyOverrides.FinalTypes {
        type Project <: anyOverrides.Project
        type CompilationUnit <: anyOverrides.CompilationUnit
        type Method <: anyOverrides.Method
        type Expression <: anyOverrides.Expression
        type TestSuite <: anyOverrides.TestSuite
        type Import <: anyOverrides.Import
        type Statement <: anyOverrides.Statement
        type Type <: anyOverrides.Type
        type Name <: anyOverrides.Name
        type ApplyExpression <: anyOverrides.ApplyExpression
        type ArgumentExpression <: anyOverrides.ArgumentExpression
      }

      trait Project extends oo.anyOverrides.Project with functional.anyOverrides.Project {
        import factory.*

        override def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[any.Method, any.Type]]): any.Project = {
          super.addTypeLookupsForMethods(lookups).addTypeLookupsForFunctions(lookups)
        }

        def copyAsScalaProject(
          compilationUnits: Set[any.CompilationUnit] = this.compilationUnits,
          customFiles: Seq[FileWithPath] = this.customFiles,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = this.constructorTypeLookupMap,
          classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type] = this.classTypeLookupMap,
          adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = this.adtTypeLookupMap,
          functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.functionTypeLookupMap,
        ): anyOverrides.Project = scalaBaseFactory.scalaProject(
          compilationUnits,
          customFiles,
          methodTypeLookupMap,
          constructorTypeLookupMap,
          classTypeLookupMap,
          adtTypeLookupMap,
          functionTypeLookupMap
        )

        override def copyAsProjectWithTypeLookups(
          compilationUnits: Set[any.CompilationUnit] = this.compilationUnits,
          customFiles: Seq[FileWithPath] = this.customFiles,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = this.constructorTypeLookupMap,
          classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type] = this.classTypeLookupMap
        ): anyOverrides.Project =
          copyAsScalaProject(
            compilationUnits = compilationUnits,
            customFiles = customFiles,
            methodTypeLookupMap = methodTypeLookupMap,
            constructorTypeLookupMap = constructorTypeLookupMap,
            classTypeLookupMap = classTypeLookupMap
          )

        override def copyAsFunctionalProject(
          compilationUnits: Set[any.CompilationUnit] = this.compilationUnits,
          customFiles: Seq[FileWithPath] = this.customFiles,
          adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = this.adtTypeLookupMap,
          functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.functionTypeLookupMap,
        ): anyOverrides.Project =
          copyAsScalaProject(
            compilationUnits = compilationUnits,
            customFiles = customFiles,
            adtTypeLookupMap = adtTypeLookupMap,
            functionTypeLookupMap = functionTypeLookupMap,
          )

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Project =
          copyAsScalaProject(
            compilationUnits = compilationUnits.map(cu => convert(cu).prefixRootPackage(rootPackageName, excludedTypeNames)),
            methodTypeLookupMap = tpeRep => methodTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            constructorTypeLookupMap = tpeRep => constructorTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            classTypeLookupMap = tpeRep => classTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            adtTypeLookupMap = tpeRep => adtTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            functionTypeLookupMap = tpeRep => functionTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
          )
      }

      trait CompilationUnit extends oo.anyOverrides.CompilationUnit with functional.anyOverrides.CompilationUnit with Util {
        import factory.*
        import functionalFactory.*
        import ooFactory.*

        def toScala: String = {
          val importDecls = imports.map(_.toScala).mkString("\n    ")
          val adtsDecl = adts.map(_.toScala).mkString("\n\n")
          val clsDecls = classes.map(_.toScala).mkString("\n\n")
          val testDecls = tests.map(_.toScala).mkString("\n\n")
          val packageDecl = if (name.init.isEmpty) "" else s"package ${name.init.map(_.toScala).mkString(".")}"
          val functionsDecl = functions.map(fun => {
            fun.copyAsClsMethod(isPublic = true).toScala
          }).mkString("\n\n")

          s"""
             |$packageDecl
             |$importDecls
             |$adtsDecl
             |$clsDecls
             |$functionsDecl
             |$testDecls
             |""".stripMargin
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.CompilationUnit = {
          copyAsScalaCompilationUnit(
            name = rootPackageName ++ name,
            imports = imports.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            methodTypeLookupMap = tpeRep => methodTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            constructorTypeLookupMap = tpeRep => constructorTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            classTypeLookupMap = tpeRep => classTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            adtTypeLookupMap = tpeRep => adtTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            functionTypeLookupMap = tpeRep => functionTypeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            classes = classes.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            adts = adts.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            functions = functions.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            tests = tests.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
          )
        }

        def copyAsScalaCompilationUnit(
          name: Seq[any.Name] = this.name,
          imports: Seq[any.Import] = this.imports,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = this.constructorTypeLookupMap,
          classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type] = this.classTypeLookupMap,
          adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = this.adtTypeLookupMap,
          functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.functionTypeLookupMap,
          classes: Seq[oo.Class] = this.classes,
          adts: Seq[functional.AlgebraicDataType] = this.adts,
          functions: Seq[any.Method] = this.functions,
          tests: Seq[any.TestSuite] = this.tests,
        ): anyOverrides.CompilationUnit = 
          scalaBaseFactory.scalaCompilationUnit(
            name,
            imports,
            methodTypeLookupMap,
            constructorTypeLookupMap,
            classTypeLookupMap,
            adtTypeLookupMap,
            functionTypeLookupMap,
            classes,
            adts,
            functions,
            tests)

        override def copyAsCompilationUnitWithClasses(
          name: Seq[any.Name] = this.name,
          imports: Seq[any.Import] = this.imports,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = this.constructorTypeLookupMap,
          classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type] = this.classTypeLookupMap,
          classes: Seq[oo.Class] = this.classes,
          tests: Seq[any.TestSuite] = this.tests,
        ): anyOverrides.CompilationUnit =
          copyAsScalaCompilationUnit(
            name = name,
            imports = imports,
            methodTypeLookupMap = methodTypeLookupMap,
            constructorTypeLookupMap = constructorTypeLookupMap,
            classTypeLookupMap = classTypeLookupMap,
            classes = classes,
            tests = tests)

        override def copyAsFunctionalCompilationUnit(
          name: Seq[any.Name] = this.name,
          imports: Seq[any.Import] = this.imports,
          adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = this.adtTypeLookupMap,
          functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.functionTypeLookupMap,
          adts: Seq[functional.AlgebraicDataType] = this.adts,
          functions: Seq[any.Method] = this.functions,
          tests: Seq[any.TestSuite] = this.tests,
        ): anyOverrides.CompilationUnit = copyAsScalaCompilationUnit(
          name = name,
          imports = imports,
          adtTypeLookupMap = adtTypeLookupMap,
          functionTypeLookupMap = functionTypeLookupMap,
          adts = adts,
          functions = functions,
          tests = tests)

        override def initializeInProject(project: any.Project): any.CompilationUnit = {
          val withLookups = copyAsScalaCompilationUnit(
            adtTypeLookupMap = project.adtTypeLookupMap,
            functionTypeLookupMap = project.functionTypeLookupMap,
            methodTypeLookupMap = project.methodTypeLookupMap,
            constructorTypeLookupMap = project.constructorTypeLookupMap,
            classTypeLookupMap = project.classTypeLookupMap,
          )
          withLookups.copyAsScalaCompilationUnit(
            tests = withLookups.tests.map(_.initializeInCompilationUnit(withLookups))
          )
        }

      }

      trait Method extends generics.anyOverrides.Method 
        with functional.anyOverrides.Method
        with funcontrol.anyOverrides.Method
        with Util {
        import factory.*
        import imperativeFactory.*
        import polymorphismFactory.*

        override def emptyPatternCtxt: funcontrol.PatternContext = functionalControlFactory.patternContext(Seq.empty)

        def addTestExpressions(exprs: Seq[any.Expression]): any.Method = {
          copy(statements = exprs.map(liftExpression))
        }

        def findClass(qualifiedName: any.Name*): any.Type =
          ooFactory.classReferenceType(qualifiedName *)

        def findMethod(qualifiedName: Seq[any.Name]): any.Expression =
          scalaBaseFactory.methodReferenceExpression(qualifiedName)

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

          s"""$mods def ${name.toScala}$typeParams($params): $returnTpe $body""".stripMargin
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Method = {
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

      trait Expression extends oo.anyOverrides.Expression {
        def toScala: String

        def isTypeReferenceExpression: Boolean = false

        def toImport: Seq[any.Import] = Seq.empty

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Expression
      }

      trait TestSuite extends oo.anyOverrides.TestSuite with Util {
        import factory.*
        import imperativeFactory.*
        import ooFactory.*
        import scalaBaseFactory.*

        def inFunSuiteStyle: oo.Class = {
          val withFunSuiteExtension =
            underlyingClass.addParent(classReferenceType(
              Seq("org", "scalatest", "funsuite", "AnyFunSuite").map(n => nameProvider.mangle(n)) *
            ))
          val methodsAsTests = withFunSuiteExtension.methods.zip(this.testMarkers).filter { case (m, isTest) => isTest }.map { case (m, _) => {
            liftExpression(applyExpression(
              applyExpression(
                memberAccessExpression(selfReferenceExpression, nameProvider.mangle("test")),
                Seq(reifiedScalaValue(TypeRep.String, m.name.component))
              ),
              Seq(blockExpression(m.statements))
            ))
          }
          }
          val withPrimaryClsConstructor = if (underlyingClass.constructors.isEmpty) {
            withFunSuiteExtension.addConstructor(constructor(statements = methodsAsTests))
          } else {
            val updatedPrimary = underlyingClass.constructors.head.copyAsConstructor(
              statements = underlyingClass.constructors.head.statements ++ methodsAsTests
            )
            withFunSuiteExtension.copy(constructors = updatedPrimary +: underlyingClass.constructors.tail)
          }
          val helperMethods = withPrimaryClsConstructor.methods.zip(testMarkers).filter { case (_, isTest) => !isTest }.map(_._1)

          withPrimaryClsConstructor.copy(methods = helperMethods)
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.TestSuite = {
          copyAsClassBasedTestSuite(underlyingClass = underlyingClass.prefixRootPackage(rootPackageName, excludedTypeNames))
        }

        def toScala: String = inFunSuiteStyle.toScala
      }

      trait Import extends any.Import {
        import factory.*
        
        def components: Seq[any.Name]

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Import = {
          if (excludedTypeNames.contains(components)) {
            this
          } else {
            copy(components = rootPackageName ++ components)
          }
        }

        def toScala: String =
          s"""import ${components.map(_.toScala).mkString(".")}"""

        def copy(components: Seq[any.Name] = this.components): any.Import =
          scalaBaseFactory.importStatement(components)
      }

      trait Statement extends any.Statement {
        def toScala: String

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Statement
      }

      trait Type extends any.Type {
        def toScala: String
        def toImport: Seq[any.Import]

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Type
      }

      trait Name extends any.Name {
        def component: String
        def mangled: String
        def toScala: String = mangled
      }

      trait ApplyExpression extends Expression with any.ApplyExpression {
        import factory.*
        def toScala: String = {
          val (typeArguments, regularArguments) = arguments.partition(_.isTypeReferenceExpression)

          // If Type arguments, then emit those without the arguments, which appear to come later
          if (typeArguments.nonEmpty) {
            val tyArgs = typeArguments.map(_.toScala).mkString("[", ", ", "]")
            s"${function.toScala}$tyArgs"
          } else {
            val args = if (regularArguments.isEmpty) "()" else regularArguments.map(_.toScala).mkString("(", ", ", ")")
            s"${function.toScala}$args"
          }

          //val result = s"${function.toScala}${tyArgs}${args}"
          //result
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.ApplyExpression = {
          copy(
            function = function.prefixRootPackage(rootPackageName, excludedTypeNames),
            arguments = arguments.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
        }
      }

      trait ArgumentExpression extends Expression with any.ArgumentExpression {
        import factory.*
        def toScala: String = s"${parameterName.toScala}"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.ArgumentExpression =
          this
      }
      
      trait Return extends Statement with any.Return {
        import factory.*
        def toScala: String = s"return { ${this.expression.toScala.stripLeading()} }"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Return =
          copy(
            expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait Factory extends oo.anyOverrides.Factory 
        with functional.anyOverrides.Factory
        with generics.anyOverrides.Factory {}
    }

    object functionalOverrides {
      trait FinalTypes extends functional.FinalTypes {
        type AlgebraicDataType <: functionalOverrides.AlgebraicDataType
        type TypeConstructor <: functionalOverrides.TypeConstructor
        type TypeInstantiationExpression <: functionalOverrides.TypeInstantiationExpression
        type ADTReferenceType <: functionalOverrides.ADTReferenceType
      }

      trait AlgebraicDataType extends functional.AlgebraicDataType with anyOverrides.Type with Util {
        import factory.*
        import functionalFactory.*

        override def toImport: Seq[any.Import] = Seq.empty

        def toScala: String = {
          val ctors = this.typeConstructors.map(_.toScala).mkString("\n  ")
          s"""
             |enum ${this.name.toScala} {
             |  $ctors
             |}""".stripMargin
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): AlgebraicDataType =
          copy(
            imports = imports.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            typeConstructors = typeConstructors.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            typeLookupMap = tpeRep => typeLookupMap(tpeRep).map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
          )
      }

      trait TypeConstructor extends functional.TypeConstructor {
        import factory.*

        def toScala: String = {
          val params = parameters.map(p => s"${p._1.toScala} : ${p._2.toScala}").mkString(",")
          s"""case ${name.toScala}($params)"""
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): functional.TypeConstructor = {
          copy(parameters = this.parameters.map(p => (p._1, p._2.prefixRootPackage(rootPackageName, excludedTypeNames))))
        }
      }

      trait TypeInstantiationExpression extends functional.TypeInstantiationExpression with anyOverrides.Expression {
        import factory.*

        override def toScala: String = {
          s"""${tpe.toScala}.${constructorName.map(_.toScala).mkString(".")}(${constructorArguments.map(_.toScala).mkString(", ")})""".stripMargin
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): functional.TypeInstantiationExpression =
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            constructorArguments = constructorArguments.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
      }

      trait ADTReferenceType extends functional.ADTReferenceType with anyOverrides.Type {
        import factory.*

        def toScala: String = qualifiedTypeName.map(_.toScala).mkString(".")

        def toImport: Seq[any.Import] = Seq.empty

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): functional.ADTReferenceType = {
          if (excludedTypeNames.contains(qualifiedTypeName)) {
            this
          } else {
            copy(
              qualifiedTypeName = rootPackageName ++ qualifiedTypeName
            )
          }
        }
      }

      trait Factory extends functional.Factory {
        override def functionalProject(
          compilationUnits: Set[any.CompilationUnit],
          customFiles: Seq[FileWithPath] = Seq.empty,
          adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = Map.empty,
          functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        ): anyOverrides.Project = scalaBaseFactory.scalaProject(
          compilationUnits = compilationUnits,
          customFiles = customFiles,
          adtTypeLookupMap = adtTypeLookupMap,
          functionTypeLookupMap = functionTypeLookupMap,
          methodTypeLookupMap = Map.empty,
          constructorTypeLookupMap = Map.empty,
          classTypeLookupMap = Map.empty,
        )

        override def funCompilationUnit(
          name: Seq[any.Name],
          imports: Seq[any.Import],
          adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = Map.empty,
          functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
          adts: Seq[functional.AlgebraicDataType] = Seq.empty,
          functions: Seq[any.Method] = Seq.empty,
          tests: Seq[any.TestSuite] = Seq.empty,
        ): anyOverrides.CompilationUnit = scalaBaseFactory.scalaCompilationUnit(
          name = name,
          imports = imports,
          methodTypeLookupMap = Map.empty,
          constructorTypeLookupMap = Map.empty,
          classTypeLookupMap = Map.empty,
          adtTypeLookupMap = adtTypeLookupMap,
          functionTypeLookupMap = functionTypeLookupMap,
          classes = Seq.empty,
          adts = adts,
          functions = functions,
          tests = tests)

      }

    }
    
    object ooOverrides {
      trait FinalTypes extends oo.FinalTypes with generics.ooOverrides.FinalTypes {
        type Class <: ooOverrides.Class
        type Constructor <: ooOverrides.Constructor
        type Field <: ooOverrides.Field
        type MemberAccessExpression <: ooOverrides.MemberAccessExpression
        type SelfReferenceExpression <: ooOverrides.SelfReferenceExpression
        type ObjectInstantiationExpression <: ooOverrides.ObjectInstantiationExpression
        type CastExpression <: ooOverrides.CastExpression
        type InstanceOfExpression <: ooOverrides.InstanceOfExpression
        type SuperReferenceExpression <: ooOverrides.SuperReferenceExpression
        type ClassReferenceType <: ooOverrides.ClassReferenceType
      }

      trait Class extends generics.ooOverrides.Class with Util {
        import factory.*
        import ooFactory.*
        import polymorphismFactory.*


        def findClass(qualifiedName: any.Name*): any.Type = {
          classReferenceType(qualifiedName *)
        }

        def classBodyDefinitionToScala: String = {
          val importDecls = imports.map(_.toScala).mkString("\n    ")
          val fieldDecls = fields.map(_.toScala).mkString("\n  ")
          val methodDecls = methods.map(_.toScala).mkString("\n  ")
          val secondaryConstructorDecls = if (constructors.isEmpty) "" else constructors.tail.map(_.toScala).mkString("\n  ")
          val initBlock = constructors.headOption.map(ctor =>
            ctor.primaryConstructorBody(this)
          ).getOrElse(fieldDecls)
          s"""
             |{
             |  $importDecls
             |  $initBlock
             |  $secondaryConstructorDecls
             |  $methodDecls
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
            ctor.parentInitializers(this)
          ).getOrElse {
            val supers = (parents ++ implemented).distinct
            if (supers.nonEmpty) supers.map(_.toScala).mkString("extends ", " with ", "") else ""
          }
          val primaryConstructorParams = constructors.headOption.map(ctor =>
            ctor.primaryConstructorParams
          ).getOrElse("")


          s"""
             |$abstractMod $kind ${name.toScala}$typeParams$primaryConstructorParams $extendsClause $classBodyDefinitionToScala""".stripMargin
        }
        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.Class = {
          copyAsGenericClass(
            imports = imports.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            typeParameters = typeParameters.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
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

      trait Constructor extends oo.Constructor with anyOverrides.Method {
        import factory.*
        import ooFactory.*

        override def typeParameters: Seq[polymorphism.TypeParameter] = Seq.empty
        override def name: any.Name = nameProvider.mangle(constructedType.map(_.toScala).getOrElse(""))
        override def returnType: Option[any.Type] = constructedType

        def importDecls: String = imports.map(_.toScala).mkString("\n    ")

        override def toScala: String = {
          val params = parameters.map(p => s"${p._1.toScala} : ${p._2.toScala}").mkString(",")
          val superInitDecls = if (superInitialization.isDefined) {
            s"// Potential Generator Defect: Scala cannot declare supers in secondary constructor"
          } else ""
          val fieldInitDecls = fieldInitializers.map(fi => {
            s"${fi._2.toScala}"
          }).mkString(", ")
          val bodyDecls =
            s"""
               |  ${statements.map(_.toScala).mkString("\n    ")}
             """.stripMargin

          s"""def this($params) = {
             |  this($fieldInitDecls)
             |  $importDecls
             |  $bodyDecls
             |}""".stripMargin
        }

        def primaryConstructorParams: String = {
          parameters.map(p => s"${p._1.toScala} : ${p._2.toScala}").mkString("(", ",", ")")
        }

        def parentInitializers(ofClass: oo.Class): String = {
          val supers = (ofClass.parents ++ ofClass.implemented).distinct
          superInitialization match {
            case Some((parent, parentArgs)) =>
              val rest = supers.filter(sp => sp != parent).map(_.toScala)
              val withRest = if (rest.isEmpty) "" else rest.mkString("with ", " with ", "")
              s"extends ${parent.toScala}(${parentArgs.map(_.toScala).mkString(", ")}) $withRest"
            case None =>
              if (supers.nonEmpty) supers.map(_.toScala).mkString("extends ", " with ", "") else ""
          }
        }

        def primaryConstructorBody(ofClass: oo.Class): String = {
          val (fieldDeclarations, remainingPrimaryInits) = {
            ofClass.fields.foldLeft[(Seq[String], Seq[(any.Name, any.Expression)])]((Seq.empty, fieldInitializers)) {
              case ((decls, remainingInitializers), declaredField) => {
                if (declaredField.init.isDefined) {
                  (decls :+ declaredField.toScala, remainingInitializers)
                } else {
                  def extractFirstDeclaration(decls: Seq[(any.Name, any.Expression)]): (Seq[(any.Name, any.Expression)], Option[any.Expression]) =
                    decls match {
                      case (name, init) +: rest if name == declaredField.name =>
                        (rest, Some(init))
                      case hd +: rest =>
                        val (remaining, init) = extractFirstDeclaration(rest)
                        (hd +: remaining, init)
                      case _ => (Seq.empty, None)
                    }

                  val (remaining, init) = extractFirstDeclaration(remainingInitializers)
                  (decls :+ declaredField.copy(init = init).toScala, remaining)
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
             |  $importDecls
             |  ${fieldDeclarations.mkString("\n  ")}
             |  $overrideInits
             |  $bodyDecls
             |  """.stripMargin
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.Constructor = {
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

      trait Field extends oo.Field {
        import factory.*

        def toScala: String = {
          val initExp = init.map(exp => s" = ${exp.toScala}").getOrElse(s" = null.asInstanceOf[${tpe.toScala}]")
          s"var ${name.toScala}: ${tpe.toScala}$initExp"
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.Field = {
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            init = init.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
          )
        }
      }

      trait MemberAccessExpression extends anyOverrides.Expression with oo.MemberAccessExpression {
        import factory.*

        def toScala: String = s"${owner.toScala}.${field.toScala}"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.MemberAccessExpression =
          copy(owner = owner.prefixRootPackage(rootPackageName, excludedTypeNames))
      }

      trait SelfReferenceExpression extends anyOverrides.Expression with oo.SelfReferenceExpression {
        def toScala: String = s"this"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.SelfReferenceExpression =
          this
      }

      trait ObjectInstantiationExpression extends anyOverrides.Expression with oo.ObjectInstantiationExpression {
        import factory.*
        import ooFactory.*

        def toScala: String = {
          val bodyScala = body.map(_.classBodyDefinitionToScala).getOrElse("")
          s"""new ${tpe.toScala}(${constructorArguments.map(_.toScala).mkString(",")})$bodyScala"""
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.ObjectInstantiationExpression =
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            constructorArguments = constructorArguments.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            body = body.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
      }

      trait CastExpression extends anyOverrides.Expression with oo.CastExpression {
        import factory.*

        def toScala: String = s"${this.expression.toScala}.asInstanceOf[${tpe.toScala}]"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.CastExpression =
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait InstanceOfExpression extends anyOverrides.Expression with oo.InstanceOfExpression {
        import factory.*

        def toScala: String = {
          s"${this.expression.toScala}.isInstanceOf[${this.tpe.toScala}]"
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.InstanceOfExpression =
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait SuperReferenceExpression extends anyOverrides.Expression with oo.SuperReferenceExpression {
        import factory.*

        def toScala: String = {
          s"super[${parentType.toScala}]"
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.SuperReferenceExpression =
          copy(
            parentType = parentType.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait ClassReferenceType extends oo.ClassReferenceType with anyOverrides.Type {
        import factory.*

        def toScala: String = qualifiedClassName.map(_.toScala).mkString(".")

        def toImport: Seq[any.Import] = Seq.empty

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): oo.ClassReferenceType = {
          if (excludedTypeNames.contains(qualifiedClassName)) {
            this
          } else {
            copy(
              qualifiedClassName = rootPackageName ++ qualifiedClassName
            )
          }
        }
      }

      trait Factory extends generics.ooOverrides.Factory {
        override def ooProject(
          compilationUnits: Set[any.CompilationUnit],
          customFiles: Seq[FileWithPath],
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = Map.empty,
          classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type] = Map.empty,
        ): anyOverrides.Project = scalaBaseFactory.scalaProject(
          compilationUnits = compilationUnits,
          customFiles = customFiles,
          adtTypeLookupMap = Map.empty,
          functionTypeLookupMap = Map.empty,
          methodTypeLookupMap = methodTypeLookupMap,
          constructorTypeLookupMap = constructorTypeLookupMap,
          classTypeLookupMap = classTypeLookupMap,
        )

        override def ooCompilationUnit(
          name: Seq[any.Name],
          imports: Seq[any.Import],
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = Map.empty,
          classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type] = Map.empty,
          classes: Seq[oo.Class] = Seq.empty,
          tests: Seq[any.TestSuite] = Seq.empty,
        ): anyOverrides.CompilationUnit = scalaBaseFactory.scalaCompilationUnit(
          name = name,
          imports = imports,
          methodTypeLookupMap = methodTypeLookupMap,
          constructorTypeLookupMap = constructorTypeLookupMap,
          classTypeLookupMap = classTypeLookupMap,
          adtTypeLookupMap = Map.empty,
          functionTypeLookupMap = Map.empty,
          classes = classes,
          adts = Seq.empty,
          functions = Seq.empty,
          tests = tests,
        )

      }
    }

    object functionalControlOverrides {
      trait FinalTypes extends funcontrol.FinalTypes {
        type Lambda <: functionalControlOverrides.Lambda
        type DeclareFunVariable <: functionalControlOverrides.DeclareFunVariable
        type FunIfThenElse <: functionalControlOverrides.IfThenElse
        type PatternMatch <: functionalControlOverrides.PatternMatch
        type PatternContext <: functionalControlOverrides.PatternContext
        type PatternVariable <: functionalControlOverrides.PatternVariable
        type ConstructorPattern <: functionalControlOverrides.ConstructorPattern
      }

      trait Lambda extends funcontrol.Lambda with anyOverrides.Expression {
        import factory.*

        def toScala: String = {
          val vars = this.variables.map { case (v, tpe) => s"${v.toScala}: ${tpe.toScala}" }.mkString("(", ", ", ")")
          val body = this.body.toScala
          s"$vars => {\n  $body \n}"
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Expression =
          copy(
            variables = this.variables.map { case (v, tpe) => (v, tpe.prefixRootPackage(rootPackageName, excludedTypeNames)) },
            body = this.body.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait DeclareFunVariable extends funcontrol.DeclareFunVariable with anyOverrides.Expression {
        import factory.*
        import functionalControlFactory.*

        def toScala: String = {
          val decl = if (this.isRecursive) "def" else "val"
          val body = this.inExp.toScala
          s"""
             |{ $decl ${this.name.toScala}: ${this.tpe.toScala} = ${this.initializer.toScala}
             |  $body }
             |""".stripMargin
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): DeclareFunVariable = {
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            initializer = initializer.prefixRootPackage(rootPackageName, excludedTypeNames),
            inExp = inExp.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
        }
      }

      trait IfThenElse extends funcontrol.IfThenElse with anyOverrides.Expression {
        import factory.*

        def toScala: String = {
          val elseIfs = elseIfBranches.map { case (condition, body) =>
            s"""
               | else if (${condition.toScala}) {
               |  ${body.toScala}
               |}""".stripMargin
          }

          s"""
             |if (${condition.toScala}) {
             |  ${this.ifBranch.toScala}
             |}${elseIfs.mkString("")} else {
             |  ${elseBranch.toScala}
             |}
                """.stripMargin
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): funcontrol.IfThenElse =
          copy(
            condition = condition.prefixRootPackage(rootPackageName, excludedTypeNames),
            ifBranch = ifBranch.prefixRootPackage(rootPackageName, excludedTypeNames),
            elseIfBranches = elseIfBranches.map { case (cond, branch) =>
              (cond.prefixRootPackage(rootPackageName, excludedTypeNames),
                branch.prefixRootPackage(rootPackageName, excludedTypeNames))
            },
            elseBranch = elseBranch.prefixRootPackage(rootPackageName, excludedTypeNames),
          )
      }

      trait PatternMatch extends funcontrol.PatternMatch with anyOverrides.Expression {
        import factory.*

        override def toScala: String = {
          val cases = this.cases.map { case (pat, body) => s"case ${pat.toScala} => { ${body.toScala} }" }.mkString("\n  ")
          s"""
             |${onValue.toScala} match {
             |  $cases
             |}""".stripMargin
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): funcontrol.PatternMatch = {
          copy(
            onValue = this.onValue.prefixRootPackage(rootPackageName, excludedTypeNames),
            cases = this.cases.map { case (pat, body) => (pat.prefixRootPackage(rootPackageName, excludedTypeNames), body.prefixRootPackage(rootPackageName, excludedTypeNames)) }
          )
        }
      }

      trait PatternContext extends funcontrol.PatternContext with Util {
      }

      trait PatternVariable extends funcontrol.PatternVariable with anyOverrides.Expression {
        import factory.*

        override def toScala: String = this.name.toScala

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): funcontrol.PatternVariable = this
      }

      trait ConstructorPattern extends funcontrol.ConstructorPattern with anyOverrides.Expression {
        import factory.*
        
        override def toScala: String = {
          s"""${tpe.toScala}.${constructor.toScala}(${arguments.map(_.toScala).mkString(", ")})"""
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): funcontrol.ConstructorPattern = {
          copy(
            tpe = this.tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            arguments = this.arguments.map(arg => arg.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
        }
      }

      trait Factory extends funcontrol.Factory {}
    }

    object polymorphismOverrides {
      trait FinalTypes extends generics.polymorphismOverrides.FinalTypes {
        type TypeReferenceExpression <: polymorphismOverrides.TypeReferenceExpression
        type TypeParameter <: polymorphismOverrides.TypeParameter
        type TypeArgument <: polymorphismOverrides.TypeArgument
        type TypeApplication <: polymorphismOverrides.TypeApplication
      }

      trait TypeReferenceExpression extends polymorphism.TypeReferenceExpression with anyOverrides.Expression {
        import factory.*
        
        def toScala: String = tpe.toScala

        override def isTypeReferenceExpression: Boolean = true

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): polymorphism.TypeReferenceExpression = {
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
        }
      }

      trait TypeParameter extends generics.polymorphismOverrides.TypeParameter {
        import factory.*

        def toScala: String = {
          val lbs =
            if (lowerBounds.nonEmpty) " <: " + lowerBounds.map(_.toScala).mkString(" with ") else ""
          val ubs =
            if (upperBounds.nonEmpty) " >: " + upperBounds.map(_.toScala).mkString(" with ") else ""
          s"""${name.toScala}$lbs$ubs"""
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): polymorphism.TypeParameter = {
          copyAsTypeParameterWithBounds(
            upperBounds = upperBounds.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
            lowerBounds = lowerBounds.map(_.prefixRootPackage(rootPackageName, excludedTypeNames)),
          )
        }
      }

      trait TypeArgument extends polymorphism.TypeArgument with anyOverrides.Type {
        import factory.*
        def toScala: String = name.toScala

        def toImport: Seq[any.Import] = Seq.empty

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): polymorphism.TypeArgument = {
          this
        }
      }

      trait TypeApplication extends polymorphism.TypeApplication with anyOverrides.Type {
        import factory.*

        def toScala: String = {
          s"${function.toScala}[${arguments.map(_.toScala).mkString(", ")}]"
        }

        def toImport: Seq[any.Import] = function.toImport ++ arguments.flatMap(_.toImport)

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): polymorphism.TypeApplication = {
          copy(
            function = function.prefixRootPackage(rootPackageName, excludedTypeNames),
            arguments = arguments.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
        }
      }

      trait Factory extends generics.polymorphismOverrides.Factory {}
    }

    object imperativeOverrides {
      trait FinalTypes extends imperative.FinalTypes {
        type DeclareVariable <: imperativeOverrides.DeclareVariable
        type AssignVariable <: imperativeOverrides.AssignVariable
        type IfThenElse <: imperativeOverrides.IfThenElse
        type While <: imperativeOverrides.While
        type VariableReferenceExpression <: imperativeOverrides.VariableReferenceExpression
      }

      trait DeclareVariable extends imperative.DeclareVariable with anyOverrides.Statement {
        import factory.*
        def toScala: String = {
          val init = this.initializer.map(ie => s" = ${ie.toScala}").getOrElse("")
          s"""
             |var ${this.name.toScala}: ${this.tpe.toScala}$init
             |""".stripMargin
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): imperative.DeclareVariable = {
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            initializer = initializer.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
        }
      }

      trait AssignVariable extends imperative.AssignVariable with anyOverrides.Statement {
        import factory.*

        def toScala: String = {
          s"""
             |${this.variable.toScala} = ${this.assignmentExpression.toScala}
             |""".stripMargin
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): imperative.AssignVariable =
          copy(assignmentExpression = assignmentExpression.prefixRootPackage(rootPackageName, excludedTypeNames))
      }

      trait IfThenElse extends imperative.IfThenElse with anyOverrides.Statement {
        import factory.*

        def toScala: String = {
          val elseIfs = elseIfBranches.map { case (condition, body) =>
            s"""
               |else if (${condition.toScala}) {
               |  ${body.map(_.toScala).mkString("\n  ")}
               |}""".stripMargin
          }

          s"""
             |if (${condition.toScala}) {
             |  ${this.ifBranch.map(_.toScala).mkString("\n  ")}
             |}${elseIfs.mkString("")} else {
             |  ${elseBranch.map(_.toScala).mkString("\n  ")}
             |}
                """.stripMargin
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): imperative.IfThenElse =
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

      trait While extends imperative.While with anyOverrides.Statement {
        import factory.*

        def toScala: String = {
          s"""
             |while (${condition.toScala}) {
             |  ${body.map(_.toScala).mkString("\n  ")}
             |}""".stripMargin
        }

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): imperative.While =
          copy(
            condition = condition.prefixRootPackage(rootPackageName, excludedTypeNames),
            body = body.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
          )
      }

      trait VariableReferenceExpression extends anyOverrides.Expression with imperative.VariableReferenceExpression {
        import factory.*

        def toScala: String = s"${name.toScala}"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): imperative.VariableReferenceExpression =
          this
      }

      trait LiftExpression extends imperative.LiftExpression with anyOverrides.Statement {
        import factory.*
        def toScala: String = s"${this.expression.toScala};"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): imperative.LiftExpression =
          copy(
            expression = expression.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }

      trait Factory extends imperative.Factory {}
    }

    trait FinalTypes {
      type ReifiedScalaValue[T] <: scalaBase.ReifiedScalaValue[T]
      type BlockExpression <: scalaBase.BlockExpression
      type MethodReferenceExpression <: scalaBase.MethodReferenceExpression
    }

    trait Util {
      import factory.*
      
      def nameProvider: NameProvider[any.Name] = nameProviderFactory.scalaNameProvider
      def reify[T](tpe: OfHostType[T], value: T): any.Expression = scalaBaseFactory.reifiedScalaValue(tpe, value)

      def findType(name: Seq[any.Name]): any.Type = functionalFactory.adtReferenceType(name *)

      def resolveImport(tpe: any.Type): Seq[any.Import] = tpe.toImport
      def resolveImport(expr: any.Expression): Seq[any.Import] = expr.toImport
      def getFreshName(basedOn: any.Name): any.Name = {
        val id = UUID.randomUUID().toString.replace("-", "")
        nameProvider.mangle(s"${basedOn.component}_$id")
      }
    }

    trait ReifiedScalaValue[T] extends anyOverrides.Expression {
      import scalaBaseFactory.*
      
      def getSelfAsReifiedScalaValue: scalaBaseFinalTypes.ReifiedScalaValue[T]
      val ofHostType: OfHostType[T]
      val value: T

      def toScala: String = {
        reificationExtensions.collectFirst(Function.unlift(ext => ext(ofHostType)(value))).getOrElse( 
          ofHostType match {
            case t: TypeRep.String.type => s""""$value""""
            case t: TypeRep.Sequence[_] =>
              value.asInstanceOf[Seq[t.elemTpe.HostType]].map(v => reifiedScalaValue(t.elemTpe, v).toScala).mkString("Seq(", ", ", ")")
            case t: TypeRep.Array[_] =>
              value.asInstanceOf[Array[t.elemTpe.HostType]].map(v => reifiedScalaValue(t.elemTpe, v).toScala).mkString("Array(", ", ", ")")
            case _ =>
              value.toString
          }
        )
      }

      override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): ReifiedScalaValue[T] =
        this
    }

    trait MethodReferenceExpression extends anyOverrides.Expression {
      import factory.*

      def getSelfAsMethodReferenceExpression: scalaBaseFinalTypes.MethodReferenceExpression

      def qualifiedMethodName: Seq[any.Name]

      def toScala: String = qualifiedMethodName.map(_.toScala).mkString(".")

      def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): MethodReferenceExpression = {
        if (excludedTypeNames.contains(qualifiedMethodName)) {
          this
        } else {
          copy(
            qualifiedMethodName = rootPackageName ++ qualifiedMethodName
          )
        }
      }

      def copy(qualifiedMethodName: Seq[any.Name] = this.qualifiedMethodName): MethodReferenceExpression =
        scalaBaseFactory.methodReferenceExpression(qualifiedMethodName)
    }

    trait BlockExpression extends anyOverrides.Expression {
      import factory.*
      
      def getSelfBlockExpression: scalaBaseFinalTypes.BlockExpression

      def statements: Seq[any.Statement] = Seq.empty

      def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): BlockExpression = {
        copy(
          statements = statements.map(_.prefixRootPackage(rootPackageName, excludedTypeNames))
        )
      }

      def toScala: String = {
        statements.map(_.toScala).mkString("{\n", "\n  ", "}")
      }

      def copy(statements: Seq[any.Statement] = this.statements): BlockExpression =
        scalaBaseFactory.blockExpression(statements)
    }

    trait Factory {
      def name(name: String, mangled: String): any.Name

      def scalaProject(
        compilationUnits: Set[any.CompilationUnit],
        customFiles: Seq[FileWithPath] = Seq.empty,
        methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = Map.empty,
        classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type] = Map.empty,
        adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = Map.empty,
        functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
      ): anyOverrides.Project

      def scalaCompilationUnit(
        name: Seq[any.Name],
        imports: Seq[any.Import],
        methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = Map.empty,
        classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type] = Map.empty,
        adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = Map.empty,
        functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        classes: Seq[oo.Class] = Seq.empty,
        adts: Seq[functional.AlgebraicDataType] = Seq.empty,
        functions: Seq[any.Method] = Seq.empty,
        tests: Seq[any.TestSuite] = Seq.empty,
      ): anyOverrides.CompilationUnit

      def importStatement(components: Seq[any.Name]): anyOverrides.Import

      def reifiedScalaValue[T](ofHostType: OfHostType[T], value: T): scalaBase.ReifiedScalaValue[T]
      def methodReferenceExpression(qualifiedMethodName: Seq[any.Name]): scalaBase.MethodReferenceExpression
      def blockExpression(statements: Seq[any.Statement]): scalaBase.BlockExpression
      
      implicit def convert[T](other: scalaBase.ReifiedScalaValue[T]): scalaBaseFinalTypes.ReifiedScalaValue[T] = other.getSelfAsReifiedScalaValue
      implicit def convert(other: scalaBase.MethodReferenceExpression): scalaBaseFinalTypes.MethodReferenceExpression = other.getSelfAsMethodReferenceExpression
      implicit def convert(other: scalaBase.BlockExpression): scalaBaseFinalTypes.BlockExpression = other.getSelfBlockExpression
    }
  }

  override val finalTypes: scalaBase.anyOverrides.FinalTypes
  override val ooFinalTypes: scalaBase.ooOverrides.FinalTypes
  override val functionalFinalTypes: scalaBase.functionalOverrides.FinalTypes
  override val functionalControlFinalTypes: scalaBase.functionalControlOverrides.FinalTypes
  override val polymorphismFinalTypes: scalaBase.polymorphismOverrides.FinalTypes
  override val imperativeFinalTypes: scalaBase.imperativeOverrides.FinalTypes
  val scalaBaseFinalTypes: scalaBase.FinalTypes

  override val factory: scalaBase.anyOverrides.Factory
  override val ooFactory: scalaBase.ooOverrides.Factory
  override val functionalFactory: scalaBase.functionalOverrides.Factory
  override val functionalControlFactory: scalaBase.functionalControlOverrides.Factory
  override val polymorphismFactory: scalaBase.polymorphismOverrides.Factory
  override val imperativeFactory: scalaBase.imperativeOverrides.Factory
  val reificationExtensions: List[(tpe: TypeRep) => (value: tpe.HostType) => Option[String]]
  val scalaBaseFactory: scalaBase.Factory
}

trait FinalBaseAST extends BaseAST {

  object finalBaseAST {
    object anyOverrides {
      trait FinalExpression extends scalaBase.anyOverrides.Expression {
        override def getSelfExpression: finalTypes.Expression = this
      }

      trait FinalStatement extends scalaBase.anyOverrides.Statement {
        def getSelfStatement: finalTypes.Statement = this
      }
    }
  }

  object finalBaseFinalTypes {
    trait FinalTypes extends scalaBase.anyOverrides.FinalTypes {
      type Project = scalaBase.anyOverrides.Project
      type CompilationUnit = scalaBase.anyOverrides.CompilationUnit
      type Method = scalaBase.anyOverrides.Method
      type Expression = finalBaseAST.anyOverrides.FinalExpression
      type TestSuite = scalaBase.anyOverrides.TestSuite
      type Import = scalaBase.anyOverrides.Import
      type Statement = scalaBase.anyOverrides.Statement
      type Type = scalaBase.anyOverrides.Type
      type Name = scalaBase.anyOverrides.Name
      type ApplyExpression = scalaBase.anyOverrides.ApplyExpression
      type ArgumentExpression = scalaBase.anyOverrides.ArgumentExpression
    }

    trait OOFinalTypes extends scalaBase.ooOverrides.FinalTypes {
      import scalaBase.ooOverrides
      type Class = ooOverrides.Class
      type Constructor = ooOverrides.Constructor
      type Field = ooOverrides.Field
      type MemberAccessExpression = ooOverrides.MemberAccessExpression
      type SelfReferenceExpression = ooOverrides.SelfReferenceExpression
      type ObjectInstantiationExpression = ooOverrides.ObjectInstantiationExpression
      type CastExpression = ooOverrides.CastExpression
      type InstanceOfExpression = ooOverrides.InstanceOfExpression
      type SuperReferenceExpression = ooOverrides.SuperReferenceExpression
      type ClassReferenceType = ooOverrides.ClassReferenceType
    }

    trait FunctionalFinalTypes extends scalaBase.functionalOverrides.FinalTypes {
      import scalaBase.functionalOverrides

      type AlgebraicDataType = functionalOverrides.AlgebraicDataType
      type TypeConstructor = functionalOverrides.TypeConstructor
      type TypeInstantiationExpression = functionalOverrides.TypeInstantiationExpression
      type ADTReferenceType = functionalOverrides.ADTReferenceType
    }

    trait FunctionalControlFinalTypes extends scalaBase.functionalControlOverrides.FinalTypes {
      import scalaBase.functionalControlOverrides

      type Lambda = functionalControlOverrides.Lambda
      type DeclareFunVariable = functionalControlOverrides.DeclareFunVariable
      type FunIfThenElse = functionalControlOverrides.IfThenElse
      type PatternMatch = functionalControlOverrides.PatternMatch
      type PatternContext = functionalControlOverrides.PatternContext
      type PatternVariable = functionalControlOverrides.PatternVariable
      type ConstructorPattern = functionalControlOverrides.ConstructorPattern
    }

    trait PolymorphismFinalTypes extends scalaBase.polymorphismOverrides.FinalTypes {
      import scalaBase.polymorphismOverrides

      type TypeReferenceExpression = polymorphismOverrides.TypeReferenceExpression
      type TypeParameter = polymorphismOverrides.TypeParameter
      type TypeArgument = polymorphismOverrides.TypeArgument
      type TypeApplication = polymorphismOverrides.TypeApplication
    }

    trait ImperativeFinalTypes extends scalaBase.imperativeOverrides.FinalTypes {
      import scalaBase.imperativeOverrides

      type DeclareVariable = imperativeOverrides.DeclareVariable
      type AssignVariable = imperativeOverrides.AssignVariable
      type IfThenElse = imperativeOverrides.IfThenElse
      type While = imperativeOverrides.While
      type VariableReferenceExpression = imperativeOverrides.VariableReferenceExpression
    }

    trait ScalaBaseFinalTypes extends scalaBase.FinalTypes {
      type ReifiedScalaValue = scalaBase.ReifiedScalaValue
      type BlockExpression = scalaBase.BlockExpression
      type MethodReferenceExpression = scalaBase.MethodReferenceExpression
    }
  }

  override val finalTypes: finalBaseFinalTypes.FinalTypes = new finalBaseFinalTypes.FinalTypes {}
  override val ooFinalTypes: finalBaseFinalTypes.OOFinalTypes = new finalBaseFinalTypes.OOFinalTypes {}
  override val functionalFinalTypes: finalBaseFinalTypes.FunctionalFinalTypes = new finalBaseFinalTypes.FunctionalFinalTypes {}
  override val functionalControlFinalTypes: finalBaseFinalTypes.FunctionalControlFinalTypes = new finalBaseFinalTypes.FunctionalControlFinalTypes {}
  override val polymorphismFinalTypes: finalBaseFinalTypes.PolymorphismFinalTypes = new finalBaseFinalTypes.PolymorphismFinalTypes {}
  override val imperativeFinalTypes: finalBaseFinalTypes.ImperativeFinalTypes = new finalBaseFinalTypes.ImperativeFinalTypes {}
  override val scalaBaseFinalTypes: finalBaseFinalTypes.ScalaBaseFinalTypes = new finalBaseFinalTypes.ScalaBaseFinalTypes {}

  object FinalBaseFactoryTypes {
    trait Factory extends scalaBase.anyOverrides.Factory {

      def returnExpression(expression: any.Expression): any.Return = {
        case class Return(override val expression: any.Expression) extends scalaBase.anyOverrides.Return with finalBaseAST.anyOverrides.FinalStatement {
        }
        Return(expression)
      }
      def applyExpression(function: any.Expression, arguments: Seq[any.Expression]): any.ApplyExpression = {
        case class ApplyExpression(
          override val function: any.Expression,
          override val arguments: Seq[any.Expression])
          extends scalaBase.anyOverrides.ApplyExpression
            with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfApplyExpression: finalTypes.ApplyExpression = this
        }
        ApplyExpression(function, arguments)
      }
      def argumentExpression(parameterName: any.Name): any.ArgumentExpression = {
        case class ArgumentExpression(override val parameterName: any.Name) extends scalaBase.anyOverrides.ArgumentExpression with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfArgumentExpression: finalTypes.ArgumentExpression = this
        }
        ArgumentExpression(parameterName)
      }
    }

    trait OOFactory extends scalaBase.ooOverrides.Factory {
      def classBasedTestSuite(underlyingClass: oo.Class, testMarkers: Seq[Boolean]): oo.anyOverrides.TestSuite = {
        class ClassBasedTestSuite(
          override val underlyingClass: oo.Class,
          override val testMarkers: Seq[Boolean]
        ) extends scalaBase.anyOverrides.TestSuite {
          override def getSelfTestSuite: finalTypes.TestSuite = this
        }
        ClassBasedTestSuite(underlyingClass, testMarkers)
      }

      def constructor(constructedType: Option[any.Type], imports: Set[any.Import], statements: Seq[any.Statement], parameters: Seq[(any.Name, any.Type)], typeLookupMap: TypeRep => Generator[any.Method, any.Type], constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type], superInitialization: Option[(any.Type, Seq[any.Expression])], fieldInitializers: Seq[(any.Name, any.Expression)]): oo.Constructor = {
        class Constructor(
          override val constructedType: Option[any.Type],
          override val imports: Set[any.Import],
          override val statements: Seq[any.Statement],
          override val parameters: Seq[(any.Name, any.Type)],
          override val typeLookupMap: TypeRep => Generator[any.Method, any.Type],
          override val constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type],
          override val superInitialization: Option[(any.Type, Seq[any.Expression])],
          override val fieldInitializers: Seq[(any.Name, any.Expression)])
        extends scalaBase.ooOverrides.Constructor {
          override def getSelfMethod: scalaBase.anyOverrides.Method = this
          override def getSelfConstructor: scalaBase.ooOverrides.Constructor = this
        }
        Constructor(constructedType, imports, statements, parameters, typeLookupMap, constructorTypeLookupMap, superInitialization, fieldInitializers)
      }

      def field(name: any.Name, tpe: any.Type, init: Option[any.Expression]): oo.Field = {
        case class Field(
          override val name: any.Name,
          override val tpe: any.Type,
          override val init: Option[any.Expression])
        extends scalaBase.ooOverrides.Field {
          override def getSelfField: scalaBase.ooOverrides.Field = this
        }
        Field(name, tpe, init)
      }
      def memberAccessExpression(owner: any.Expression, field: any.Name): oo.MemberAccessExpression = {
        case class MemberAccessExpression(
          override val owner: any.Expression,
          override val field: any.Name)
        extends scalaBase.ooOverrides.MemberAccessExpression
        with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfMemberAccessExpression: scalaBase.ooOverrides.MemberAccessExpression = this
        }
        MemberAccessExpression(owner, field)
      }
      def objectInstantiationExpression(tpe: any.Type, constructorArguments: Seq[any.Expression], body: Option[oo.Class]): oo.ObjectInstantiationExpression = {
        case class ObjectInstantiationExpression(
          override val tpe: any.Type,
          override val constructorArguments: Seq[any.Expression],
          override val body: Option[oo.Class])
        extends scalaBase.ooOverrides.ObjectInstantiationExpression
        with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfObjectInstantiationExpression: scalaBase.ooOverrides.ObjectInstantiationExpression = this
        }
        ObjectInstantiationExpression(tpe, constructorArguments, body)
      }
      def castExpression(tpe: any.Type, expression: any.Expression): oo.CastExpression = {
        case class CastExpression(
          override val tpe: any.Type,
          override val expression: any.Expression)
        extends scalaBase.ooOverrides.CastExpression
        with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfCastExpression: scalaBase.ooOverrides.CastExpression = this
        }
        CastExpression(tpe, expression)
      }
      def instanceOfExpression(tpe: any.Type, expression: any.Expression): oo.InstanceOfExpression = {
        case class InstanceOfExpression(
          override val tpe: any.Type,
          override val expression: any.Expression)
        extends scalaBase.ooOverrides.InstanceOfExpression
        with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfInstanceOfExpression: scalaBase.ooOverrides.InstanceOfExpression = this
        }
        InstanceOfExpression(tpe, expression)
      }
      def superReferenceExpression(parentType: any.Type): oo.SuperReferenceExpression = {
        case class SuperReferenceExpression(
          override val parentType: any.Type)
        extends scalaBase.ooOverrides.SuperReferenceExpression
        with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfSuperReferenceExpression: scalaBase.ooOverrides.SuperReferenceExpression = this
        }
        SuperReferenceExpression(parentType)
      }
      def selfReferenceExpression: oo.SelfReferenceExpression = {
        case class SelfReferenceExpression()
        extends scalaBase.ooOverrides.SelfReferenceExpression
        with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfSelfReferenceExpression: scalaBase.ooOverrides.SelfReferenceExpression = this
        }
        SelfReferenceExpression()
      }
      def classReferenceType(qualifiedClassName: any.Name*): oo.ClassReferenceType = {
        case class ClassReferenceType(
          override val qualifiedClassName: any.Name*)
        extends scalaBase.ooOverrides.ClassReferenceType {
          override def getSelfType: scalaBase.anyOverrides.Type = this
          override def getSelfClassReferenceType: scalaBase.ooOverrides.ClassReferenceType = this
        }
        ClassReferenceType(qualifiedClassName*)
      }
    }

    trait FunctionalFactory extends scalaBase.functionalOverrides.Factory {
      def adt(name: any.Name,
        imports: Seq[any.Import],
        typeConstructors: Seq[functional.TypeConstructor],
        typeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type]): functional.AlgebraicDataType = {
        case class ADT(override val name: any.Name,
          override val imports: Seq[any.Import],
          override val typeConstructors: Seq[functional.TypeConstructor],
          override val typeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type])
        extends scalaBase.functionalOverrides.AlgebraicDataType {
          def getSelfAlgebraicDataType: functionalFinalTypes.AlgebraicDataType = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        ADT(name, imports, typeConstructors, typeLookupMap)
      }
      def typeConstructor(name: any.Name, parameters: Seq[(any.Name, any.Type)]): functional.TypeConstructor = {
        case class TypeConstructor(override val name: any.Name,
          override val parameters: Seq[(any.Name, any.Type)]
        ) extends scalaBase.functionalOverrides.TypeConstructor {
          def getSelfTypeConstructor: functionalFinalTypes.TypeConstructor = this
        }
        TypeConstructor(name, parameters)
      }
      def typeInstantiationExpression(tpe: any.Type, constructorName: Seq[any.Name], constructorArguments: Seq[any.Expression]): functional.TypeInstantiationExpression = {
        case class TypeInstantiationExpression(
          override val tpe: any.Type,
          override val constructorName: Seq[any.Name],
          override val constructorArguments: Seq[any.Expression])
          extends scalaBase.functionalOverrides.TypeInstantiationExpression
          with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfTypeInstantiationExpression: functionalFinalTypes.TypeInstantiationExpression = this
        }
        TypeInstantiationExpression(tpe, constructorName, constructorArguments)
      }
      def adtReferenceType(qualifiedTypeName: any.Name*): functional.ADTReferenceType = {
        case class AdtReferenceType(override val qualifiedTypeName: any.Name*)
          extends scalaBase.functionalOverrides.ADTReferenceType {
          def getSelfADTReferenceType: functionalFinalTypes.ADTReferenceType = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        AdtReferenceType(qualifiedTypeName*)
      }
    }
    
    trait FunctionalControlFactory extends scalaBase.functionalControlOverrides.Factory {
      def lambda(variables: Seq[(any.Name, any.Type)], body: any.Expression): funcontrol.Lambda = {
        case class Lambda(
          override val variables: Seq[(any.Name, any.Type)],
          override val body: any.Expression
        ) extends scalaBase.functionalControlOverrides.Lambda
        with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfLambda: functionalControlFinalTypes.Lambda = this
        }
        Lambda(variables, body)
      }
      def declareFunVariable(name: any.Name, tpe: any.Type, isRecursive: Boolean, initializer: any.Expression, inExp: any.Expression): funcontrol.DeclareFunVariable = {
        case class DeclareFunVariable(
          override val name: any.Name,
          override val tpe: any.Type,
          override val isRecursive: Boolean,
          override val initializer: any.Expression,
          override val inExp: any.Expression
        ) extends scalaBase.functionalControlOverrides.DeclareFunVariable
        with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfDeclareFunVariable: functionalControlFinalTypes.DeclareFunVariable = this
        }
        DeclareFunVariable(name, tpe, isRecursive, initializer, inExp)
      }
      def funIfThenElse(condition: any.Expression, ifBranch: any.Expression, elseIfBranches: Seq[(any.Expression, any.Expression)], elseBranch: any.Expression): funcontrol.IfThenElse = {
        case class FunIfThenElse(
          override val condition: any.Expression,
          override val ifBranch: any.Expression,
          override val elseIfBranches: Seq[(any.Expression, any.Expression)],
          override val elseBranch: any.Expression
        ) extends scalaBase.functionalControlOverrides.IfThenElse
        with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfFunIfThenElse: functionalControlFinalTypes.FunIfThenElse = this
        }
        FunIfThenElse(condition, ifBranch, elseIfBranches, elseBranch)
      }
      def patternContext(variables: Seq[any.Name]): funcontrol.PatternContext = {
        case class PatternContext(
          override val variables: Seq[any.Name]
        ) extends scalaBase.functionalControlOverrides.PatternContext {
          def getSelfPatternContext: functionalControlFinalTypes.PatternContext = this
        }
        PatternContext(variables)
      }
      def patternVariable(name: any.Name): funcontrol.PatternVariable = {
        case class PatternVariable(
          override val name:any.Name
        ) extends scalaBase.functionalControlOverrides.PatternVariable
        with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfPatternVariable: functionalControlFinalTypes.PatternVariable = this
        }
        PatternVariable(name)
      }
      def constructorPattern(tpe: any.Type, constructor: any.Name, arguments: Seq[any.Expression]): funcontrol.ConstructorPattern = {
        case class ConstructorPattern(
          override val tpe: any.Type,
          override val constructor: any.Name,
          override val arguments: Seq[any.Expression]
        ) extends scalaBase.functionalControlOverrides.ConstructorPattern
        with finalBaseAST.anyOverrides.FinalExpression {
          override def getSelfConstructorPattern: functionalControlFinalTypes.ConstructorPattern = this
        }
        ConstructorPattern(tpe, constructor, arguments)
      }
      def patternMatch(onValue: any.Expression, cases: Seq[(any.Expression, any.Expression)]): funcontrol.PatternMatch = {
        case class PatternMatch(
          override val onValue: any.Expression,
          override val cases: Seq[(any.Expression, any.Expression)]
        ) extends scalaBase.functionalControlOverrides.PatternMatch
        with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfPatternMatch: functionalControlFinalTypes.PatternMatch = this
        }
        PatternMatch(onValue, cases)
      }
    }

    trait PolymorphismFactory extends scalaBase.polymorphismOverrides.Factory {
      def typeArgument(name: any.Name): polymorphism.TypeArgument = {
        case class TypeArgument(override val name: any.Name) extends scalaBase.polymorphismOverrides.TypeArgument {
          def getSelfTypeArgument: polymorphismFinalTypes.TypeArgument = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        TypeArgument(name)
      }
      def typeApplication(function: any.Type, arguments: Seq[any.Type]): polymorphism.TypeApplication = {
        case class TypeApplication(override val function: any.Type, override val arguments: Seq[any.Type]) extends scalaBase.polymorphismOverrides.TypeApplication {
          def getSelfTypeApplication: polymorphismFinalTypes.TypeApplication = this
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        TypeApplication(function, arguments)
      }
      def typeReferenceExpression(tpe: any.Type): polymorphism.TypeReferenceExpression = {
        case class TypeReferenceExpression(override val tpe:any.Type)
          extends scalaBase.polymorphismOverrides.TypeReferenceExpression
            with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfTypeReferenceExpression: polymorphismFinalTypes.TypeReferenceExpression = this
        }
        TypeReferenceExpression(tpe)
      }
    }

    trait GenericsFactory extends generics.Factory {
      def genericClass(name: any.Name, imports: Seq[any.Import], typeParameters: Seq[polymorphism.TypeParameter], parents: Seq[any.Type], implemented: Seq[any.Type], fields: Seq[oo.Field], methods: Seq[any.Method], constructors: Seq[oo.Constructor], methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type], constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type], typeLookupMap: TypeRep => Generator[oo.Class, any.Type], isAbstract: Boolean, isInterface: Boolean, isStatic: Boolean): generics.ooOverrides.Class = {
        class GenericClass(
          override val name: any.Name,
          override val imports: Seq[any.Import],
          override val typeParameters: Seq[polymorphism.TypeParameter],
          override val parents: Seq[any.Type],
          override val implemented: Seq[any.Type],
          override val fields: Seq[oo.Field],
          override val methods: Seq[any.Method],
          override val constructors: Seq[oo.Constructor],
          override val methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type],
          override val constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type],
          override val typeLookupMap: TypeRep => Generator[oo.Class, any.Type],
          override val isAbstract: Boolean,
          override val isInterface: Boolean,
          override val isStatic: Boolean
        ) extends scalaBase.ooOverrides.Class {
          override def getSelfClass: scalaBase.ooOverrides.Class = this
        }
        GenericClass(name, imports, typeParameters, parents, implemented, fields, methods, constructors, methodTypeLookupMap, constructorTypeLookupMap, typeLookupMap, isAbstract, isInterface, isStatic)
      }
      def genericMethod(name: any.Name, imports: Set[any.Import], statements: Seq[any.Statement], returnType: Option[any.Type], typeParameters: Seq[polymorphism.TypeParameter], parameters: Seq[(any.Name, any.Type)], typeLookupMap: TypeRep => Generator[any.Method, any.Type], isAbstract: Boolean, isStatic: Boolean, isPublic: Boolean, isOverride: Boolean): generics.anyOverrides.Method = {
        class GenericMethod(override val name: any.Name,
          override val imports: Set[any.Import],
          override val statements: Seq[any.Statement],
          override val returnType: Option[any.Type],
          override val typeParameters: Seq[polymorphism.TypeParameter],
          override val parameters: Seq[(any.Name, any.Type)],
          override val typeLookupMap: TypeRep => Generator[any.Method, any.Type],
          override val isAbstract: Boolean, isStatic: Boolean,
          override val isPublic: Boolean,
          override val isOverride: Boolean) extends scalaBase.anyOverrides.Method {
          override def getSelfMethod: scalaBase.anyOverrides.Method = this
        }
        GenericMethod(name, imports, statements, returnType, typeParameters, parameters, typeLookupMap, isAbstract, isStatic, isPublic, isOverride)
      }
      def typeParameterWithBounds(name: any.Name, upperBounds: Seq[any.Type], lowerBounds: Seq[any.Type]): generics.polymorphismOverrides.TypeParameter = {
        class TypeParameterWithBounds(
          override val name: any.Name,
          override val upperBounds: Seq[any.Type],
          override val  lowerBounds: Seq[any.Type]
        ) extends scalaBase.polymorphismOverrides.TypeParameter {
          override def getSelfTypeParameter: polymorphismFinalTypes.TypeParameter = this
        }
        TypeParameterWithBounds(name, upperBounds, lowerBounds)
      }
    }

    trait ImperativeFactory extends scalaBase.imperativeOverrides.Factory {
      def declareVariable(name: any.Name, tpe: any.Type, initializer: Option[any.Expression]): imperative.DeclareVariable = {
        case class DeclareVariable(
          override val name: any.Name,
          override val tpe: any.Type,
          override val initializer: Option[any.Expression]
        ) extends scalaBase.imperativeOverrides.DeclareVariable with finalBaseAST.anyOverrides.FinalStatement {
          def getSelfDeclareVariable: imperativeFinalTypes.DeclareVariable = this
        }
        DeclareVariable(name, tpe, initializer)
      }
      def variableReferenceExpression(name: any.Name): imperative.VariableReferenceExpression = {
        case class VariableReferenceExpression(override val name: any.Name)
          extends scalaBase.imperativeOverrides.VariableReferenceExpression
          with finalBaseAST.anyOverrides.FinalExpression {
            def getSelfVariableReferenceExpression: imperativeFinalTypes.VariableReferenceExpression = this
        }
        VariableReferenceExpression(name)
      }
      def assignVariable(variable: any.Expression, expression: any.Expression): imperative.AssignVariable = {
        case class AssignVariable(override val variable: any.Expression, override val assignmentExpression: any.Expression)
          extends scalaBase.imperativeOverrides.AssignVariable
          with finalBaseAST.anyOverrides.FinalStatement {
          override def getSelfStatement: scalaBase.anyOverrides.Statement = this
          override def getSelfAssignVariable: imperativeFinalTypes.AssignVariable = this
        }
        AssignVariable(variable, expression)
      }
      // cannot find LiftExpression in scalaBase.*.LiftExpression
      def liftExpression(expression: any.Expression): imperative.LiftExpression = {
        case class LiftExpression(override val expression:any.Expression)
          extends scalaBase.imperativeOverrides.LiftExpression  // NEEDS TO BE OVERRIDE
          with finalBaseAST.anyOverrides.FinalStatement {
        }
        LiftExpression(expression)
      }
      def ifThenElse(condition: any.Expression,
                     ifBranch: Seq[any.Statement],
                     elseIfBranches: Seq[(any.Expression, Seq[any.Statement])],
                     elseBranch: Seq[any.Statement]): imperative.IfThenElse = {
        case class IfThenElse(
          override val condition: any.Expression,
          override val ifBranch: Seq[any.Statement],
          override val elseIfBranches: Seq[(any.Expression, Seq[any.Statement])],
          override val elseBranch: Seq[any.Statement])
          extends scalaBase.imperativeOverrides.IfThenElse
          with finalBaseAST.anyOverrides.FinalStatement {
          def getSelfIfThenElse: imperativeFinalTypes.IfThenElse = this
        }
        IfThenElse(condition, ifBranch, elseIfBranches, elseBranch)
      }
      def whileLoop(condition: any.Expression, body: Seq[any.Statement]): imperative.While = {
        case class WhileLoop(override val condition: any.Expression, override val body: Seq[any.Statement])
          extends scalaBase.imperativeOverrides.While
          with finalBaseAST.anyOverrides.FinalStatement {
          def getSelfWhile: imperativeFinalTypes.While = this
        }
        WhileLoop(condition,body)
      }
    }
    
    trait ScalaBaseFactory extends scalaBase.Factory {
      def name(name: String, mangled: String): any.Name = {
        case class Name(override val component: String, override val mangled: String) extends scalaBase.anyOverrides.Name {
          def getSelfName: scalaBase.anyOverrides.Name = this
        }
        Name(name, mangled)
      }
      def scalaProject(compilationUnits: Set[any.CompilationUnit], customFiles: Seq[FileWithPath], methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type], constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type], classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type], adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type], functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type]): scalaBase.anyOverrides.Project = {
        // removed case class to avoid multiple copy implementations
        class ScalaProject(
          override val compilationUnits: Set[any.CompilationUnit],
          override val customFiles: Seq[FileWithPath],
          override val methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type],
          override val constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type],
          override val classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type],
          override val adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type],
          override val functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type]
        ) extends scalaBase.anyOverrides.Project {
          def getSelfProject: scalaBase.anyOverrides.Project = this
        }
        ScalaProject(compilationUnits, customFiles, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap, adtTypeLookupMap, functionTypeLookupMap)
      }
      def scalaCompilationUnit(name: Seq[any.Name],
                               imports: Seq[any.Import],
                               methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type],
                               constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type],
                               classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type],
                               adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type],
                               functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type],
                               classes: Seq[oo.Class],
                               adts: Seq[functional.AlgebraicDataType],
                               functions: Seq[any.Method],
                               tests: Seq[any.TestSuite]):  scalaBase.anyOverrides.CompilationUnit = {
        class ScalaCompilationUnit(override val name: Seq[any.Name],
                                        override val imports: Seq[any.Import],
                                        override val methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type],
                                        override val constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type],
                                        override val classTypeLookupMap: TypeRep => Generator[oo.Class, any.Type],
                                        override val adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type],
                                        override val functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type],
                                        override val classes: Seq[oo.Class],
                                        override val adts: Seq[functional.AlgebraicDataType],
                                        override val functions: Seq[any.Method],
                                        override val tests: Seq[any.TestSuite])
          extends scalaBase.anyOverrides.CompilationUnit {
          def getSelfCompilationUnit: scalaBase.anyOverrides.CompilationUnit = this
        }
        ScalaCompilationUnit(name, imports, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap, adtTypeLookupMap, functionTypeLookupMap, classes, adts, functions, tests)
      }
      def importStatement(components: Seq[any.Name]): scalaBase.anyOverrides.Import = {
        case class ImportStatement(override val components: Seq[any.Name]) extends scalaBase.anyOverrides.Import {
          def getSelfImport: scalaBase.anyOverrides.Import = this
        }
        ImportStatement(components)
      }
      def reifiedScalaValue[T](ofHostType: OfHostType[T], value: T): scalaBase.ReifiedScalaValue[T] = {
        case class ReifiedScalaValue(override val ofHostType: OfHostType[T], override val value: T)
          extends scalaBase.ReifiedScalaValue[T]
          with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfAsReifiedScalaValue: scalaBaseFinalTypes.ReifiedScalaValue[T] = this
        }
        ReifiedScalaValue(ofHostType, value)
      }
      def methodReferenceExpression(qualifiedMethodName: Seq[any.Name]): scalaBase.MethodReferenceExpression = {
        case class MethodReferenceExpression(override val qualifiedMethodName: Seq[any.Name])
          extends scalaBase.MethodReferenceExpression 
          with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfAsMethodReferenceExpression: scalaBaseFinalTypes.MethodReferenceExpression = this
        }
        MethodReferenceExpression(qualifiedMethodName)
      }
      def blockExpression(statements: Seq[any.Statement]): scalaBase.BlockExpression = {
        case class BlockExpression (override val statements: Seq[any.Statement])
          extends scalaBase.BlockExpression
          with finalBaseAST.anyOverrides.FinalExpression {
          def getSelfBlockExpression: scalaBaseFinalTypes.BlockExpression = this
        }
        BlockExpression(statements)
      }
    }
  }

  override val factory: scalaBase.anyOverrides.Factory = new FinalBaseFactoryTypes.Factory {}
  override val ooFactory: scalaBase.ooOverrides.Factory = new FinalBaseFactoryTypes.OOFactory {}
  override val functionalFactory: scalaBase.functionalOverrides.Factory = new FinalBaseFactoryTypes.FunctionalFactory {}
  override val functionalControlFactory: scalaBase.functionalControlOverrides.Factory = new FinalBaseFactoryTypes.FunctionalControlFactory {}
  override val polymorphismFactory: scalaBase.polymorphismOverrides.Factory = new FinalBaseFactoryTypes.PolymorphismFactory {}
  override val imperativeFactory: scalaBase.imperativeOverrides.Factory = new FinalBaseFactoryTypes.ImperativeFactory {}
  override val genericsFactory: generics.Factory = new FinalBaseFactoryTypes.GenericsFactory {}
  override val scalaBaseFactory: scalaBase.Factory = new FinalBaseFactoryTypes.ScalaBaseFactory {}
}
