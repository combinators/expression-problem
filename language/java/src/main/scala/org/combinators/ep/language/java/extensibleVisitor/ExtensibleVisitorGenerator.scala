package org.combinators.ep.language.java.extensibleVisitor   /*DI:LD:AD*/

import com.github.javaparser.ast.body.{ConstructorDeclaration, MethodDeclaration, TypeDeclaration}
import org.combinators.ep.domain.{BaseDomain, ModelDomain, OperationDependency}
import org.combinators.ep.language.java.visitor.VisitorGenerator
import org.combinators.templating.twirl.Java

/**
  * Synthesizing OO and Functional Design to promote Reuse
  * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
  * https://dl.acm.org/citation.cfm?id=679709
  *
  * TODO: Doesn't yet work for c1 merged, since it reuses code from visitor (constructors)
  * that need to be modified instead
  */
trait ExtensibleVisitorGenerator extends VisitorGenerator with OperationDependency {
  val domain:BaseDomain with ModelDomain

  /**
    * Generating a visitor solution requires:
    *
    * 1. A Class for every data type
    * 2. A Class for every operation
    * 3. Abstract Base class and visitor class
    * @return
    */
  override def generatedCode():Seq[CompilationUnit] = {
    val flat = getModel.flatten()

    //  binary methods for helper
    val decls:Seq[CompilationUnit] = if (flat.hasBinaryMethod) {
      generateHelperClasses()
    } else {
      Seq.empty
    }

    val includeBinaryMethod = flat.hasBinaryMethod

    decls ++ getModel.inChronologicalOrder.flatMap(m =>
      m.types.map(tpe => generateExtensibleExp(includeBinaryMethod, m, tpe)) ++       // one for each type; important to pass in both 'flat' and 'm'
        m.ops.map(op => generateOperation(m, op))                      // and new operations
    ) ++
      // cannot have extension for the FIRST model entry, so skip it
      getModel.inChronologicalOrder
          .filter(m => m.types.nonEmpty)
          .flatMap(m => m.last.pastOperations()
        .map(op => operationExtension(op, m))) ++  // don't forget past operations
      getModel.inChronologicalOrder
           .filter(m => m.types.nonEmpty)
           .map(m => generateBase(m))  :+           // visitor gets its own class (overriding concept)
      generateBaseClass(flat.ops, "extensibleVisitor")                 // abstract base class
  }


  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      dispatch(Java("e").expression(), delta.op.get, delta.params : _ *)
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /**
    * Add virtual type generator. Context is either "" for top level operation, or the most recent one.
    *
    * Defect found: Must be sure to instantiate the most recent context, hence the suffix context.
    */
  def addVirtualConstructorSubtype(mainType:TypeDeclaration[_], op:domain.Operation, context:String) : Unit = {
    val virtualConstructor = Java(
      s"""|${op.concept}$context make${op.concept} (${parameters(op)}) {
          |  return new ${op.concept}$context (${arguments(op)});
          |}""".stripMargin).methodDeclarations().head

    mainType.addMember(virtualConstructor)
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.accept(make${op.concept}($args))""").expression()
  }

  /**
    * Use run-time validation to confirm, but only needed after first base level...
    *
    * Even though super-class method uses flatten, we cannot do so, because of the
    * requirement that "we only add visitor checks for models after first one."
    */
   def generateExtensibleExp(includeBinaryMethod:Boolean, model:domain.Model, exp:domain.DataType) : CompilationUnit = {
    val unit = generateExp(includeBinaryMethod, exp, "extensibleVisitor")

    // replace old accept method with new one
    val klass = unit.getType(0)
    val acceptMethod:MethodDeclaration = klass.getMethodsByName("accept").get(0)

    // only for models after the first one...
    if (model.last.equals(domain.emptyModel())) { return unit }

    val full = model.types.sortWith(_.name < _.name).mkString("")

    val newBody:MethodDeclaration = Java(s"""
          |public <R> R accept(Visitor<R> v) {
          | 	if (v instanceof Visitor$full) {
          | 		return ((Visitor$full<R>)v).visit(this);
          | 	}
          | 	throw new RuntimeException ("Older visitor used with newer datatype variant.");
          |}""".stripMargin).methodDeclarations().head

    acceptMethod.setBody(newBody.getBody.get)

    unit
  }

  /** Concatenate all types in this model to form proper suffix for operation classes. */
  def modelTypes(model:domain.Model) : String = {
    if (model.last.equals(domain.emptyModel())) {
      ""
    } else {
      model.types.sortWith(_.name < _.name).mkString("")
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type in past. */
  def generateBase(model:domain.Model): CompilationUnit = {
    val methods:Seq[MethodDeclaration] = model.types.flatMap(exp => Java(s"public R visit(${exp.name} exp);").methodDeclarations())
    val full:String = modelTypes(model)

    val parent:Option[String] = if (model.last.equals(domain.emptyModel())) {
      None
    } else {
      val prior:String = modelTypes(model.last.lastModelWithDataTypes()) // might not be good enough.
      Some(s"Visitor$prior<R>")
    }

    addMethods(makeInterface("extensibleVisitor", s"Visitor$full<R>", Seq.empty, parent), methods)
  }

  /**
    * Pulled out since useful in both visitor AND extensible visitor, where it is overridden
    * to take advantage of knowledge of the model within which op is defined.
    *
    * If a producer method
    */
  override def generateConstructor (op:domain.Operation, model:domain.Model): String = {
    val full:String = modelTypes(model)
    if (op.parameters.isEmpty) {
      ""
    } else {
      val option = if (full.isEmpty) { None} else { Some(op.concept + full) }
      constructorFromOp(op, option).toString
    }
  }

  /** Extensions based on past operation */
  def operationExtension(op:domain.Operation, model:domain.Model): CompilationUnit = {
    val regularVisitor:CompilationUnit = super.generateVisitorOperation(model, op)

    val opType:Type = typeConverter(op.returnType.get)
    val full:String = modelTypes(model)
    val lastWithType  = model.last.lastModelWithDataTypes()
    val lastOperation = lastWithType.findOperation(op)

    // must take care to ensure we don't mistakenly go back *before* where the operation was defined. This
    // is determined by looking for operations in past.
    val last = if (lastWithType == model.base || lastOperation.isEmpty) {
      ""
    } else {
      modelTypes(lastWithType)
    }

    val replacement = makeClass("extensibleVisitor", s"${op.concept}$full", Seq(s"Visitor$full<$opType>"), Some(s"${op.concept}$last"))
    // copy everything over from the originally generated class
    val newType = replacement.getType(0)
    copyDeclarations(regularVisitor.getType(0), newType)

    val elements = newType.getMembers.iterator()

    // any constructors have to have appropriate super invocations
    while (elements.hasNext) {
      elements.next match {
        case constr:ConstructorDeclaration =>
          constr.getBody.addStatement(0, superFromOp(op))
        case _ =>
      }
    }
    addVirtualConstructorSubtype(newType, op, full)

    replacement
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  def generateOperation(model:domain.Model, op:domain.Operation): CompilationUnit = {
    val regularVisitor:CompilationUnit = super.generateVisitorOperation(model, op)
    val mainType:TypeDeclaration[_] = regularVisitor.getType(0)

    // convert 'extends visitor' into 'implements visitor'
    // rename class to have types at end (except for first)
    val opType = if (op.returnType.isEmpty) {
      "Void"   // generics
    } else {
      typeConverter(op.returnType.get)
    }
    val full:String = modelTypes(model)

    val fullVisitor:String = if (model.types.nonEmpty) {
      modelTypes(model)
    } else {
      modelTypes(model.lastModelWithDataTypes())
    }

    val replacement:CompilationUnit =
       addMethods(makeClass("extensibleVisitor", s"${op.concept}$full", Seq(s"Visitor$fullVisitor<$opType>")),
         model.last.pastDataTypes().map(exp => methodGenerator(exp, op)))

    val newType = replacement.getType(0)
    copyDeclarations(mainType, newType)


    // dependent operations here; must be sure that the context for dependent operations
    // is based on the actual operation itself (and not just full).
    addVirtualConstructorSubtype(newType, op, full)

    dependency(op).foreach(op => {
      // we have to be more careful and grab the most recently generated
      // subclass for the given operation, but then check to see if any
      // later ones have added subtypes (and if so, then use them).
      var models = model.inChronologicalOrder
      var selected:Option[domain.Model] = None
      while (models.nonEmpty) {
        val m = models.head
        if (m.ops.contains(op)) {
          // we have found the one containing the operation; if
          // any future model declares types, then they must be used.
          selected = Some(m)
        } else if (selected.isDefined) {
          if (m.types.nonEmpty) {
            selected = Some(m)
          }
        }
        models = models.tail
      }

      val opFull = modelTypes(selected.get)
      addVirtualConstructorSubtype(newType, op, opFull)
    })

    replacement
  }
}
