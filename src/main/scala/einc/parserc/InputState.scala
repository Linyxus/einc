package einc.parserc

case class InputState(str: String, pos: SourcePos):
  override def toString(): String =
    val idx = pos.idx
    val (l, r) = str.splitAt(idx)
    s"InputState($l|$r)"

object InputState:
  def fromString(str: String): InputState = 
    InputState(str, SourcePos(0))
