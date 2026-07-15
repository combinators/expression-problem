package org.combinators.equals

import org.combinators.cogen.TypeRep

/** Represents a Scala model of an instance of the given domain specific data type. */

// need downcast from Object to the class
// for each field, knows == or .equals or Arrays.equals()
// if not in-built type, then we are declaring so must rely on .equals()

trait DataType {
  
}

case class CompositeDataType(val name:String, fields:Map[String, DataType]) extends DataType {
  
}

case class BuiltInDataType(val typeRep:TypeRep) extends DataType {
  
}

// note: at no point do we need to discuss arrays, because: we have FFI CoGen handles array equality correctly.
// Hidden benefits of FFIs as well as CoCo design pattern. 

// to get working with Scala, still would need to have InBetween. 
//
// by placing in inbetween. We have imperative API for generating AST.
// java bridges gap manually because of java github parser.
// for inbetween we bridge that gap once (generically). Then for each AST node, tell it how to translate that AST 
// node into the target language. No longer have to connect EACH and EVERY command to the AST and you can focus 
// only on the differences.