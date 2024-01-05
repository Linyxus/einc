package einc.core

trait Error extends Positioned:
  def message: String
  def addenda: List[String]

trait NotationError extends Error

