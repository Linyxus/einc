package einc.core

import einc.parserc.SourcePos

case class Span(start: Int, length: Int):
  assert(length >= 0)
  def --(other: Span): Span =
    val start1 = start min (other.start)
    val thisEnd = start + length
    val otherEnd = other.start + other.length
    val end1 = thisEnd max otherEnd
    Span(start1, end1 - start1)

  def --(other: SourcePos): Span =
    Span(start, other.pos - start)

object Span:
  def fromSourcePos(start: SourcePos, finish: SourcePos): Span =
    Span(start.pos, finish.pos - start.pos)

