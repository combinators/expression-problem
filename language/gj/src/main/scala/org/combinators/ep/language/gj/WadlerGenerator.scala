package org.combinators.ep.language.gj      /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.domain.BaseDomain

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait WadlerGenerator extends GJGenerator  {

  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel:domain.Model

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. A Base class to be superclass of them all
    */
  def generatedCode():Seq[GJWithPath] = {
   getModel.inChronologicalOrder.tail.flatMap(m => generateLang(m)) ++  // skip first one
      generateBase(getModel.inChronologicalOrder.head)                  // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    GJ(s"${att.name}")
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    // This is how visitor pattern does it. Wadler's original email uses 'this'
    // GJ(s"$expr.visit(new ${op.concept}())")
    GJ(s"$expr.visit(this)")
  }

  /** Return designated GJ type associated with type, or void if all else fails. */
  override def typeConverter(tpe:TypeRep) : Type = {
    tpe match {
      case domain.baseTypeRep => new GJType(s"This.${domain.baseTypeRep.name}")
      case domain.Void => new GJType("void")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = typeConverter(op.returnType)

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:DataType, op:Operation): GJ = {
    val params = parametersExp(exp)
    GJ(s"""|public ${returnType(op)} for${exp.concept}($params) {
           |  ${logic(exp, op).mkString("\n")}
           |}""".stripMargin)
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:Model, exp:DataType) : GJ = {
    val args = exp.attributes.map(att => att.instance).mkString(",")
    val visitMethod = GJ(s"""|public <R> R visit(This.Visitor<R> v) {
                             |      return v.for${exp.concept}($args);
                             |}""".stripMargin)

    GJ(s"""|class ${exp.concept} implements ${domain.baseTypeRep.name} {
           |  ${fields(exp).mkString("\n")}
           |  $visitMethod
           |  ${constructor(exp)}
           |}""".stripMargin)
  }

  /** Generate the full class for the given operation. */
  def generateOp(model:Model, op:Operation) : GJ = {
    val methods = model.types.map(exp => methodGenerator(exp, op))

    GJ(s"""|class ${op.concept} implements Visitor<${returnType(op)}> {
           |  ${methods.mkString("\n")}
           |}""".stripMargin)
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateLang(model:Model): Seq[GJWithPath] = {

    // base will be assumed to have at least one datatype exp and one e
    val baseMethods = model.types.map(exp => {
      val params:String =  exp.attributes.map(att => GJ(s"${typeConverter(att.tpe)} ${att.instance}")).mkString(",")
      GJ(s"public R for${exp.concept}($params);")
    })

    val typeClasses = model.types.map(exp => generateExp(model, exp))
    val opClasses = model.ops.map(op => generateOp(model, op))

    val extendsClause = s"extends Lang_${model.last.name}F<This>"

    // GJ(s"protected final ${typeConverter(att.tpe, covariantOverride)} ${att.name}_;"
    val code = GJ(s"""|class Lang_${model.name}F<This extends Lang_${model.name}F<This>> $extendsClause {
                      |  interface Visitor<R> {
                      |    ${baseMethods.mkString("\n")}
                      |  }
                      |  interface ${domain.baseTypeRep.name} {
                      |    public <R> R visit(This.Visitor<R> v);
                      |  }
                      |
                      |  ${typeClasses.mkString("\n")}
                      |  ${opClasses.mkString("\n")}
                      |}""".stripMargin)

    Seq(GJWithPath(code, Paths.get(s"Lang_${model.name}F.gj")),
      GJWithPath(GJ(s"final class Lang_${model.name} extends LangF<Lang_${model.name}> {}"), Paths.get(s"Lang_${model.name}.gj"))
    )
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(base:Model): Seq[GJWithPath] = {
    // base will be assumed to have at least one datatype exp and one e
    val baseMethods = base.types.map(exp => {
      val params:String =  exp.attributes.map(att => GJ(s"${typeConverter(att.tpe)} ${att.instance}")).mkString(",")
      GJ(s"public R for${exp.concept}($params);")
    })

    val typeClasses = base.types.map(exp => generateExp(base, exp))

    val opClasses = base.ops.map(op => generateOp(base, op))

    val code = GJ(s"""|class Lang_${base.name}F<This extends Lang_${base.name}F<This>> {
                      |  interface Visitor<R> {
                      |    ${baseMethods.mkString("\n")}
                      |  }
                      |  interface ${domain.baseTypeRep.name} {
                      |    public <R> R visit(This.Visitor<R> v);
                      |  }
                      |
                      |  ${typeClasses.mkString("\n")}
                      |  ${opClasses.mkString("\n")}
                      |}""".stripMargin)

    Seq(GJWithPath(code, Paths.get(s"Lang_${base.name}F.gj")),
      GJWithPath(GJ(s"final class Lang_${base.name} extends LangF<Lang_${base.name}> {}"), Paths.get(s"Lang_${base.name}.gj"))
    )
  }
}
