package einc.core

import einc.parserc.SourcePos

case class Span(start: Int, length: Int)

object Span:
  def fromSourcePos(start: SourcePos, finish: SourcePos): Span =
    Span(start.pos, finish.pos - start.pos)

