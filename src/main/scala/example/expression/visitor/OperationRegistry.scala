package example.expression.visitor

import expression.{DomainModel, Exp, Operation}

import scala.collection.JavaConverters._

 /**
  * Given Exp, register an appropriate Exp seq of statements for the given operation
  */
abstract class OperationRegistry(op:Operation) {
  def register(e: Exp): Unit

  /** Register all data types in the domain Model. */
  def customize(m: DomainModel): Unit = m.data.asScala.foreach(exp =>  register(exp))

}
