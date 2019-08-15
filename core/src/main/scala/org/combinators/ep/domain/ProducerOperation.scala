package org.combinators.ep.domain    /*DI:LI:AI*/

class ProducerOperation(val baseTypeRep:TypeRep.Aux[Inst], override val name:String, override val parameters:Seq[Parameter] = Seq.empty)
  extends Operation(name, baseTypeRep, parameters)

