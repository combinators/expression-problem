package example.expression.cpp

trait InstanceContext {
  var _context_id: Int = 0

  /** increment and return next id. */
  def next_id: Int = {
    _context_id = _context_id + 1

    _context_id
  }

  /**
    * For building up instance structure we need context which can be used to assemble a symbol table
    */
  abstract class Context {

    val contextId: Int = next_id
    var past: List[Context] = List.empty

    def add(ctx: Context): Context = {
      past = past :+ ctx

      this
    }

    /** Return context id for definition in variables. */
    def id: Int = contextId

    /** Return definition statement in C++ for this variable */
    def toStatement: String

    /** Return type of variable. */
    def tpe: String

    /** Return name of variable. */
    def name: String

    /** Return variable reference, i.e., "&double9" or "&mult3"). */
    def varReference: String = "&" + name + id
  }

  /**
    * Invalid context
    *
    * Use on off-chance that context is invalid.
    */
  final class EmptyContext(val _type:String) extends Context {
    override val name: String = ""
    override val tpe: String = _type

    override def toStatement: String = s"// Invalid type (" + tpe + ")"
  }

  /**
    * When declaring a lit
    *
    * double dvar = 8;
    * Lit two   = Lit(&dvar);
    *
    * name = dvar, type= double, value = 8
    */
  final class LitContext(val _name: String, val _type: String, val _value: Double) extends Context {
    override val name: String = _name
    override val tpe: String = _type

    val value: Double = _value

    override def toStatement: String = {
      val dbl = s"double val$id = $value;"
      val total =
        s"""
           |$dbl
           |$tpe  ${tpe.toLowerCase}$id = $tpe(&val$id);
         """.stripMargin

      total
    }
  }

  /**
    * When declaring a binary expression
    *
    * Mult mvar  = Mult(&one, &two);
    *
    * name = mvar, type= Mult, left = &one, right=&two
    */
  final class BinaryContext(val _name: String, val _type: String, val _left: Context, val _right: Context) extends Context {
    val name: String = _name
    val tpe: String = _type
    val left: Context = _left
    val right: Context = _right

    // add contexts
    add(_left)
    add(_right)

    override def toStatement: String = {
      val leftDef = left.toStatement
      val rightDef = right.toStatement
      val total = s"""
         |$leftDef
         |$rightDef
         |$tpe  ${tpe.toLowerCase}$id = $tpe(${left.varReference}, ${right.varReference});
       """.stripMargin

      total
    }
  }


  /**
    * When declaring a unary expression
    *
    * Neg nvar  = Neg(&one);
    *
    * name = nvar, type= Neg, value = &one
    */
  final class UnaryContext(val _name: String, val _type: String, val _value: Context) extends Context {
    val name: String = _name
    val tpe: String = _type
    val value: Context = _value

    // add contexts
    add(_value)

    override def toStatement: String = s"$tpe  ${tpe.toLowerCase}$id = $tpe(${value.varReference});"
  }

}