package org.combinators.ep.language.java

/*DI:LD:AI*/

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{ConstructorDeclaration, FieldDeclaration, MethodDeclaration, TypeDeclaration}
import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.BlockStmt
import com.github.javaparser.ast.{Modifier, body}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.DomainIndependentGenerator
import org.combinators.templating.twirl.Java

import scala.collection.JavaConverters._

/*
/**
  * Any Java-based EP approach can extend this Generator.
  *
  * @groupname lang Language Bindings
  * @groupdesc lang Fundamental Language Bindings as required by EpCoGen framework
  * @groupprio lang 5
  * @groupname api Core API
  * @groupdesc api Fundamental abstractions to provide context for [[DomainIndependentGenerator]]
  * @groupname api Core API
  * @groupname deltaHelpers DeltaHelpers
  * @groupname context Context
  * @groupname types Parameterized Types
  * @groupname dependency External Dependencies
  * @groupname inst Instantiating data types
  * @groupdesc dependency Depends upon BaseDomain (for the core logic) and the desired
  * @groupdesc api Fundamental abstractions needed for any language-based solution to EP
  * @groupprio api 0
  * @groupdesc types Each language must define relevant abstractions that map to these types.
  *            It is acceptable if the same structure is used for multiple types (as an example,
  *            review CPPElement)
  * @groupprio types 10
  * @groupdesc context Each language and approach needs different solutions to assemble the logic
  *           for a given (data-type and operation). The top-level concepts are shown here.
  * @groupprio types 20
  * @groupdesc inst When generating test cases, it is essential to include construction
  *            code that constructs instances of the data types. In addition, some receursive
  *            operations depend on being able to constrct instances of data types.
  * @groupprio context 30
  * @groupdesc deltaHelpers When weaving together code expressions representing partial fragments
  *           for a given logic, these helper methods are useful in capturing the desired structures.
  * @groupprio deltaHelpers 40
  */
 */

/** Provides the base class for domain independent Java code genertors. */
abstract class DomainIndependentJavaGenerator extends DomainIndependentGenerator(JavaSyntax) {

  import JavaNameProvider._
  import syntax._

  /** @inheritdoc */
  override def tpe(tpe: TypeRep): Type = {
    tpe match {
      case TypeRep.Double => Java("Double").tpe
      case TypeRep.Int => Java("Integer").tpe
      case TypeRep.String => Java("String").tpe
      case TypeRep.Unit => Java("void").tpe
      case dt@TypeRep.DataType(_) => Java(conceptNameOf(dt)).tpe
      case _ => super.tpe(tpe)
    }
  }

  /** @inheritdoc */
  override def toOperationResult(expr: syntax.Expression): Seq[syntax.Statement] =
    Java(s"return $expr;").statements()

  /** Generates a constructor for the given data type case.
    *
    * Transforms the data type case name with the given function.
    * Uses `identity` as the default name transformation, leaving names intact.
    *
    * Generated constructors initialize all data type case attributes as fields with values taken as parameters.
    *
    * Example:
    * {{{
    *   constructor(DataTypeCase("Foo", Attribute("x", TypeRep.Int))), name => "Bar" + name)
    *   // result:
    *   // public BarFoo(int x) { this.x = x; }
    * }}}
    */
  def constructor(tpeCase: DataTypeCase, transformCaseName: String => String = identity): ConstructorDeclaration = {
    val name = conceptNameOf(tpeCase.copy(name = transformCaseName(tpeCase.name)))
    val params: Seq[String] = tpeCase.attributes.map(att => s"${tpe(att.tpe)} ${mangle(att.name)}")
    val initialization: Seq[Statement] =
      tpeCase
        .attributes
        .map(att => Java(s"this.${mangle(att.name)} = ${mangle(att.name)};").statement())

    Java(
      s"""|public $name (${params.mkString(",")}) {
          |    ${initialization.mkString("\n    ")}
          |}""".stripMargin
    ).constructors().head
  }

  /** Generates a constructor for the given operation.
    *
    * Transforms the data operation name with the given function.
    * Uses `identity` as the default name transformation, leaving names intact.
    *
    * Generated constructors initialize all operation parameters as fields with values taken as parameters.
    *
    * Example:
    * {{{
    *   constructor(Operation("Foo", Seq(Parameter("x", TypeRep.Int)))), name => "Bar" + name)
    *   // result:
    *   // public BarFoo(int x) { this.x = x; }
    * }}}
    */
  def constructor(op: Operation, transformOpName: String => String = identity): ConstructorDeclaration = {
    val name = conceptNameOf(op.copy(name = transformOpName(op.name)))
    val params: Seq[body.Parameter] = parameters(op)
    val initialization: Seq[Statement] =
      op
        .parameters
        .map(param =>
          Java(s"this.${mangle(param.name)} = ${mangle(param.name)};").statement()
        )

    Java(
      s"""|public $name (${params.mkString(",")}) {
          |    ${initialization.mkString("\n    ")}
          |}""".stripMargin
    ).constructors().head
  }

  /** Generates list of parameter names for the given operation. */
  def arguments(op: Operation): Seq[SimpleName] =
    op.parameters.map(param => Java(mangle(param.name)).simpleName)

  /** Generates comma-separated list of parameter declarations ("Type name") for the given operation. */
  def parameters(op: Operation): Seq[body.Parameter] =
    op.parameters.map(param =>
      new body.Parameter(tpe(param.tpe), Java(mangle(param.name)).simpleName)
    )

  /** Generates public getter methods for all attributes of the given data type case. */
  def getters(tpeCase: DataTypeCase): Seq[MethodDeclaration] =
    tpeCase.attributes.flatMap { att =>
      Java(
        s"""|public ${tpe(att.tpe)} get${conceptNameOf(att.tpe)}() {
            |    return this.${mangle(att.name)};
            |}""".stripMargin
      ).methodDeclarations()
    }

  /** Generates private fields for all attributes of a data type case. */
  def fields(tpeCase: DataTypeCase): Seq[FieldDeclaration] =
    tpeCase.attributes.flatMap { att =>
      Java(s"private ${tpe(att.tpe)} ${mangle(att.name)};").fieldDeclarations()
    }

  /** Generates a [[CompilationUnit]] with exactly one empty public class in it.
    *
    * @param pkgName    Name of package into which class belongs (None for the default package)
    * @param className  Desired class name
    * @param implements Sequence of implemented interfaces
    * @param superclass Potential superclass
    */
  def makeClass(
    pkgName: Option[String],
    className: SimpleName,
    implements: Seq[Type] = Seq.empty,
    superclass: Option[ClassOrInterfaceType] = None
  ): CompilationUnit = {
    val cpUnit = pkgName.map(new CompilationUnit(_)).getOrElse(new CompilationUnit())
    val cls = cpUnit.addClass(className.asString, Modifier.PUBLIC)
    implements.foreach { implemented =>
      cls.addImplementedType(implemented.asString)
    }
    if (superclass.nonEmpty) {
      cls.addExtendedType(superclass.get.clone)
    }

    cpUnit
  }

  /** Generates a [[CompilationUnit]] with exactly one empty public interface in it.
    *
    * @param pkgName       Name of package into which class belongs (None for the default package)
    * @param interfaceName Desired interface name
    * @param extended      Sequence of parent interfaces
    */
  def makeInterface(
    pkgName: Option[String],
    interfaceName: SimpleName,
    extended: Seq[Type]
  ): CompilationUnit = {
    val cpUnit = makeClass(pkgName, interfaceName)
    val interface = cpUnit.getClassByName(interfaceName.asString).get
    interface.setInterface(true)
    extended.foreach { parent =>
      interface.addExtendedType(parent.asString)
    }

    cpUnit
  }

  /** Adds methods to a type declared in the given [[CompilationUnit]].
    *
    * The compilation unit may only declare one type.
    * No check is made to determine whether duplicate methods would exist.
    */
  def addMethods(unit: CompilationUnit, methods: Seq[MethodDeclaration]): CompilationUnit = {
    require(unit.getTypes.size == 1, "Only one type may be declared in the compilation unit.")

    val newUnit = unit.clone
    val clazz = newUnit.getType(0)
    methods.foreach(m => clazz.addMember(m))

    newUnit
  }

  /** Adds a Javadoc-style comment to the first type of the CompilationUnit. */
  def addTypeComment(unit: CompilationUnit, doc: String): CompilationUnit = {
    val newUnit = unit.clone
    newUnit.getType(0).setComment(JavaParser.parseJavadoc(doc).toComment(""))
    newUnit
  }

  /** Appends all members declared in `oldType` to the members declared in `newType`. */
  def copyDeclarations[T <: TypeDeclaration[_]](oldType: TypeDeclaration[_], newType: T): T = {
    val resultType = newType.clone()
    val members = oldType.getMembers.iterator().asScala
    members.foreach { member =>
      resultType.addMember(member.clone)
    }
    resultType.asInstanceOf[T] // hack: clone is a producer method ;)
  }

  /** Adds statements to the end of the given method. */
  def appendStatements(method: MethodDeclaration, stmts: Seq[Statement]): MethodDeclaration = {
    val newMethod = method.clone()
    if (!newMethod.getBody.isPresent) {
      val bb: BlockStmt = new BlockStmt()
      newMethod.setBody(bb)
    }
    val block = newMethod.getBody.get

    stmts.foreach(s => block.addStatement(s))
    newMethod.setBody(block)
    newMethod
  }
}