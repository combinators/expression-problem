package org.combinators.ep.generator.communication

/** Provides abstractions for communicating with instances of domain specific data types. */

import org.combinators.ep.domain.abstractions._

/** Models a request to perform operation `op` supplying arguments for each of its parameters.
 *
 * A runtime assertion checks that arguments are provided for all parameters of the operation.
 */
case class Request[Expression](op: Operation, arguments: Map[Parameter, Expression]) {
  require(op.parameters.forall(arguments.isDefinedAt),
    s"Missing argument for parameter ${op.parameters.find(!arguments.isDefinedAt(_)).get} of operation ${op.name}"
  )
}

/** Models a received request to perform an operation on a data type.
 *
 * At the time of delivery, the approach and language specific dispatch mechanism has filled in the data type case.
 * It also provides expressions to access the receiving instances (e.g. "this" in OO) and its attributes
 * (e.g. "this.left").
 *
 * A runtime assertion checks that access to all attributes of the data type case is provided.
 */
case class ReceivedRequest[Expression](
                                        onType: DataType,
                                        tpeCase: DataTypeCase,
                                        selfReference: Expression,
                                        attributes: Map[Attribute, Expression],
                                        request: Request[Expression]
                                      ) {
  require(tpeCase.attributes.forall(attributes.isDefinedAt),
    s"Missing accessor expression for attribute ${tpeCase.attributes.find(!attributes.isDefinedAt(_)).get} of data type case ${tpeCase.name}"
  )
}

/** Models a potential request that is not yet directed to an instance of a data type but COULD be directed to given case of the data type. */
case class PotentialRequest(
                             onType: DataType,
                             tpeCase: DataTypeCase,
                             op: Operation
                           )

/** Models sending a request to some instance of a data type.
 *
 * At the time of sending, the data type case is unknown and will be resolved by language and approach specific
 * dispatch code.
 *
 * When the message is sent while handling another message (inside the domain-specific code generated for an
 * operation), it should include its request information in the `inReplyTo` field.
 */
case class SendRequest[Expression](
                                    to: Expression,
                                    receiverTpe: DataType,
                                    request: Request[Expression],
                                    inReplyTo: Option[ReceivedRequest[Expression]] = None // TODO: models what was essentially done with Deltas in the past
                                  )

