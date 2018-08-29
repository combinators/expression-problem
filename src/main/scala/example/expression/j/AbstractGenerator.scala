package example.expression.j  /*DI:LD:AI*/

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.body.{ConstructorDeclaration, FieldDeclaration, MethodDeclaration, TypeDeclaration}
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.LanguageIndependentGenerator
import org.combinators.templating.twirl.Java

import scala.collection.JavaConverters._

/**
  * Any Java-based EP approach can extend this Generator
  */
trait AbstractGenerator extends LanguageIndependentGenerator with DependentDispatch {
  val domain:BaseDomain with ModelDomain

  type CompilationUnit = com.github.javaparser.ast.CompilationUnit
  type Type = com.github.javaparser.ast.`type`.Type
  type Expression = com.github.javaparser.ast.expr.Expression
  type Statement = com.github.javaparser.ast.stmt.Statement

  // Useful helper methods for any generator needing to craft common Java constructs

  /** Generate constructor for given atomic concept, using suggested name */
  def constructor(exp:domain.Atomic, suggestedName:Option[String] = None, covariantOverride:Option[Type] = None) : ConstructorDeclaration = {
    val name = if (suggestedName.isEmpty) { exp.name } else { suggestedName.get }

    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${typeConverter(att.tpe, covariantOverride)} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att => s"${typeConverter(att.tpe, covariantOverride)} ${att.name}")
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    Java(s"""|public $name (${params.mkString(",")}) {
             |   ${cons.mkString("\n")}
             |}""".stripMargin).constructors().head
  }

  /** Generate constructor for given operation, using suggested name */
  def constructorFromOp(op:domain.Operation, suggestedName:Option[String] = None) : ConstructorDeclaration = {
    val name = if (suggestedName.isEmpty) { op.name.capitalize } else { suggestedName.get }

    val params:Seq[String] = op.parameters.map(p => s"${typeConverter(p._2)} ${p._1}")
    val cons:Seq[Statement] = op.parameters.flatMap(p => Java(s"  this.${p._1} = ${p._1};").statements())

    Java(
      s"""|public $name (${params.mkString(",")}) {
          |   ${cons.mkString("\n")}
          |}""".stripMargin).constructors().head
  }

  /** Compute parameter "name" comma-separated list from operation. */
  def arguments(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1

       name
    }).mkString(",")
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.TypeRep = tuple._2

      typeConverter(tpe).toString + " " + name
    }).mkString(",")
  }

  /**
    * Produce all getter methods for the given exp, with suitable possibiity of using covariant replacement
    * on domain.BaseTypeRep
    */
  def getters(exp:domain.Atomic, covariantOverride:Option[Type] = None) : Seq[MethodDeclaration] =

    exp.attributes.flatMap(att => Java(s"""|public ${typeConverter(att.tpe, covariantOverride)} get${att.name.capitalize}() {
                                           |    return this.${att.name};
                                           |}""".stripMargin).methodDeclarations())

  /**
    * Given an exp, produce a sequence of field declarations, for each of the attributes.
    *
    * @param exp
    * @param covariantOverride    Optional cass to use as covariant overriding class for BaseTypeExp
    * @return
    */
  def fields(exp:domain.Atomic, covariantOverride:Option[Type] = None) : Seq[FieldDeclaration] = {
    exp.attributes.flatMap(att => Java(s"private ${typeConverter(att.tpe, covariantOverride)} ${att.name};").fieldDeclarations())
  }

  /**
    * Return a new class with designated superclass and potential implements clauses
    * @param pkgName      Name of package into which class belongs (if "" then default package)
    * @param className    Desired class name
    * @param implements   sequence of strings representing interfaces
    * @param superclass   potential superclass
    * @return
    */
  def makeClass(pkgName:String, className:String, implements:Seq[String] = Seq.empty, superclass:Option[String] = None) : CompilationUnit = {
    val packageClause = if (pkgName == "") {
      ""
    } else {
      s"package $pkgName;"
    }

    val implementsClause:String = if (implements.isEmpty) {
      ""
    } else {
      "implements " + implements.mkString(",")
    }

    val extendsClause:String = if (superclass.isEmpty) {
      ""
    } else {
      "extends " + superclass.get
    }
    Java(s"""|$packageClause
             |public class $className $extendsClause $implementsClause {
             |
             |}""".stripMargin).compilationUnit
  }

  /**
    * Return a new class with designated superclass and potential implements clauses
    * @param pkgName      Name of package into which class belongs (if "" then default package)
    * @param className    Desired class name
    * @param implements   sequence of strings representing interfaces
    * @param parentInterface   potential superclass
    * @return
    */
  def makeInterface(pkgName:String, className:String, implements:Seq[String] = Seq.empty, parentInterface:Option[String] = None) : CompilationUnit = {
    val packageClause = if (pkgName == "") {
      ""
    } else {
      s"package $pkgName;"
    }

    val implementsClause:String = if (implements.isEmpty) {
      ""
    } else {
      "implements " + implements.mkString(",")
    }

    val extendsClause:String = if (parentInterface.isEmpty) {
      ""
    } else {
      "extends " + parentInterface.get
    }
    Java(s"""|$packageClause
             |public interface $className $extendsClause $implementsClause {
             |
             |}""".stripMargin).compilationUnit
  }

  /**
    * Add methods to the class.
    *
    * No check is made to determine whether duplicate methods would exist.
    *
    * @param unit       Class to be modified
    * @param methods    Methods to be added.
    * @return
    */
  def addMethods(unit:CompilationUnit, methods:Seq[MethodDeclaration]) : CompilationUnit = {
    val clazz = unit.getType(0)
    methods.foreach(m => clazz.addMember(m))

    unit
  }

  /**
    * Add Javadoc-style comment to primary type of CompilationUnit
    *
    * @param unit       CompilationUnit to be modified
    * @param doc        Comment to be applied as javadoc to the given type
    * @param typeIndex  Which type (defaults to 0) to have javadoc modified.
    * @return
    */
  def addTypeComment(unit:CompilationUnit, doc:String, typeIndex:Int = 0) : CompilationUnit = {
    unit.getType(typeIndex).setComment(JavaParser.parseJavadoc(doc).toComment(""))
    unit
  }


  def copyDeclarations (oldType:TypeDeclaration[_], newType:TypeDeclaration[_]) : Unit = {
    val elements = oldType.getMembers.iterator().asScala
    while (elements.hasNext) {
      newType.addMember(elements.next)
    }
  }
}
