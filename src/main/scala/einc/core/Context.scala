package einc.core

case class Context(errors: List[Error])

object Context:
  def ctx(using Context): Context = summon

