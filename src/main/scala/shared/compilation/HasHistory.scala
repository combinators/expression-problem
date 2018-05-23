package shared.compilation

import expression.history.History

/**
  * Offers a history
  */
trait HasHistory {

  def history: History
}
