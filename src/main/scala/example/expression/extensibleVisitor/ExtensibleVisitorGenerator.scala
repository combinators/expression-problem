package example.expression.extensibleVisitor    /*DI:LD:AD*/

import com.github.javaparser.ast.body.{MethodDeclaration, TypeDeclaration}
import example.expression.domain.{BaseDomain, ModelDomain, OperationDependency}
import example.expression.scalaVisitor.VisitorGenerator
import org.combinators.templating.twirl.Java

/**
  * Synthesizing OO and Functional Design to promote Reuse
  * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
  * https://dl.acm.org/citation.cfm?id=679709
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
    val flat:domain.Model = getModel.flatten()

    //  binary methods for helper
    val decls:Seq[CompilationUnit] = if (flat.ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ getModel.inChronologicalOrder.flatMap(m =>
      m.types.map(tpe => generateExp(m, tpe)) ++               // one for each type; use 'flat' to ensure we detect binary methods
        m.ops.map(op => operationGenerator(m, op))             // and new operations
    ) ++
      // cannot have extension for the FIRST model entry, so skip it
      getModel.inChronologicalOrder
          .filter(m => m.types.nonEmpty)
          .flatMap(m => m.last.pastOperations()
        .map(op => operationExtension(op, m))) ++  // don't forget past operations
      getModel.inChronologicalOrder
           .filter(m => m.types.nonEmpty)
           .map(m=> generateBase(m))  :+      // visitor gets its own class (overriding concept)
      generateBaseClass(getModel)                                  // abstract base class
  }

  /**
    * Responsible for delegating to a new operation on the current context.
    */
  def recreate(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val m:domain.Model = getModel // getModel.findType(exp)
    val opargs = params.mkString(",")

    val full = if (m.last.equals(domain.emptyModel())) {
      ""
    } else {
      m.types.sortWith(_.name < _.name).mkString("")
    }

    //Java(s"e.accept(new ${op.name.capitalize}$full($opargs))").expression[Expression]()
    Java(s"e.accept(make${op.name.capitalize}($opargs))").expression[Expression]()
  }

  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      recreate(source.exp.get, delta.op.get)
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
      s"""|${op.name.capitalize} make${op.name.capitalize} (${parameters(op)}) {
          |  return new ${op.name.capitalize}$context (${arguments(op)});
          |}""".stripMargin).methodDeclarations().head

    mainType.addMember(virtualConstructor)
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.accept(make${op.name.capitalize}($args))""").expression()
  }



  /** Use run-time validation to confirm, but only needed after first base level... */
  override def generateExp(model:domain.Model, exp:domain.Atomic) : CompilationUnit = {
    val unit = super.generateExp(model, exp)

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

  /** Contactenate all types in this model to form proper suffix for operation classes. */
  def modelTypes(model:domain.Model) : String = {
    if (model.last.equals(domain.emptyModel())) {
      ""
    } else {
      model.types.sortWith(_.name < _.name).mkString("")
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type in past. */
  override def generateBase(model:domain.Model): CompilationUnit = {
    val methods:Seq[MethodDeclaration] = model.types.flatMap(exp => Java(s"public R visit(${exp.name} exp);").methodDeclarations())
    val full:String = modelTypes(model)

    val parent:Option[String] = if (model.last.equals(domain.emptyModel())) {
      None
    } else {
      val prior:String = modelTypes(model.last.lastModelWithDataTypes()) // might not be good enough.
      Some(s"Visitor$prior<R>")
    }

    val unit = addMethods(makeInterface("expression", s"Visitor$full<R>", Seq.empty, parent), methods)
    addTypeComment(unit, s"""
                           |A concrete visitor describes a concrete operation on expressions. There is one visit
                           |method per type in the class hierarchy.
                          """.stripMargin)
  }


  /** Extensions based on past operation */
  def operationExtension(op:domain.Operation, model:domain.Model): CompilationUnit = {
    val regularVisitor:CompilationUnit = super.operationGenerator(model, op)

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

    val replacement = makeClass("expression", s"${op.name.capitalize}$full", Seq(s"Visitor$full<$opType>"), Some(s"${op.name.capitalize}$last"))

    // copy everything over from the originally generated class
    val newType = replacement.getType(0)
    copyDeclarations(regularVisitor.getType(0), newType)
    addVirtualConstructorSubtype(newType, op, full)

    replacement
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  override def operationGenerator(model:domain.Model, op:domain.Operation): CompilationUnit = {
    val regularVisitor:CompilationUnit = super.operationGenerator(model, op)
    val mainType:TypeDeclaration[_] = regularVisitor.getType(0)

    // convert 'extends visitor' into 'implements visitor'
    // rename class to have types at end (except for first)
    val opType = op match {
      case bmb:domain.BinaryMethodTreeBase => Java(s"tree.Tree").tpe()
      case _ => typeConverter(op.returnType.get)
    }

    val full:String = modelTypes(model)

    val fullVisitor:String = if (model.types.nonEmpty) {
      modelTypes(model)
    } else {
      modelTypes(model.lastModelWithDataTypes())
    }

    val replacement:CompilationUnit =
       addMethods(makeClass("expression", s"${op.name.capitalize}$full", Seq(s"Visitor$fullVisitor<$opType>")),
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
