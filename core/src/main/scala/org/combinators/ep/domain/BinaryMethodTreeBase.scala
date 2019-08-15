package org.combinators.ep.domain    /*DI:LI:AI*/

class BinaryMethodTreeBase(val baseTypeRep:TypeRep.Aux[Inst], override val name:String, override val returnType:TypeRep)
  extends Operation(name, baseTypeRep)
