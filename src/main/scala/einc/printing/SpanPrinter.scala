package einc.printing

import einc.core.*

class SpanPrinter(source: String, contextLines: Int = 0, marker: Char = '^', markerMore: Char = '~') extends Printer:
  val sourceLines: List[String] = source.linesWithSeparators.toList

  def locatePos(idx: Int): (Int, Int) =
    var lino = 0
    var col = 0
    @annotation.tailrec def go(rest: Int): (Int, Int) =
      if rest <= 0 then (lino, col)
      else if lino >= sourceLines.length then (lino, 0)
      else
        val line = sourceLines(lino)
        if col >= line.length - 1 then
          lino += 1
          col = 0
          go(rest - 1)
        else
          col += 1
          go(rest - 1)
    go(idx)

  def show(span: Span, msg: String, addenda: List[String]): String =
    val Span(start, length) = span
    val (startLino, startCol) = locatePos(start)
    val (endLino, endCol) = locatePos(start + length)

    val maxLino = (endLino + contextLines).min(sourceLines.length - 1)
    val linoWidth = maxLino.toString.length
    val prefixLen = 4 + linoWidth

    def getLine(lino: Int): String =
      if lino < sourceLines.length then sourceLines(lino)
      else ""
    def showLine(lino: Int): String =
      val linoStr = leftPad((lino + 1).toString, linoWidth, padding = ' ')
      var line = getLine(lino)
      if !line.endsWith("\n") then
        line = line + "\n"
      s" $linoStr | $line"

    val sb = StringBuilder()
    def printContextLine(lino: Int): Unit =
      sb ++= showLine(lino)
    def printMainLine(lino: Int, markerStart: Int, markerLen: Int, firstLine: Boolean): Unit =
      val prefix = " " * (prefixLen + markerStart)
      val markerStr = if firstLine then marker.toString else markerMore.toString
      val moreLen = 0.max(markerLen - 1)
      val moreStr = markerMore.toString * moreLen
      val markerLine = s"$prefix$markerStr$moreStr"

      sb ++= showLine(lino)
      sb ++= markerLine
      sb ++= "\n"
      if firstLine then
        sb ++= s"$prefix$msg\n"

    val ctxStart = 0.max(startLino - contextLines)
    val ctxEnd = (sourceLines.length - 1).min(endLino + contextLines)

    // (1) Context lines before main lines
    (ctxStart until startLino).foreach: lino =>
      printContextLine(lino)

    // (2) The first main line
    val startMarkerLen =
      if startLino == endLino then
        endCol - startCol
      else
        0.max(getLine(startLino).length - startCol)
    printMainLine(startLino, startCol, startMarkerLen, firstLine = true)

    // (3) The middle main lines
    (startLino + 1).until(endLino).foreach: lino =>
      printMainLine(lino, 0, getLine(lino).length, firstLine = false)

    // (4) The last line
    if endLino > startLino then
      printMainLine(endLino, 0, endCol, firstLine = false)

    // (5) The addenda
    addenda.foreach: line =>
      sb ++= " " * 2
      sb ++= line
      if !line.endsWith("\n") then
        sb ++= "\n"

    // (6) Context lines after main lines
    (endLino + 1).to(ctxEnd).foreach: lino =>
      printContextLine(lino)

    sb.result()

