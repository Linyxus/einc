package einc.printing

trait Printer:
  def leftPad(s: String, width: Int, padding: Char = ' '): String =
    var delta = width - s.length
    if delta < 0 then delta = 0
    padding.toString * delta + s

  def rightPad(s: String, width: Int, padding: Char = ' '): String =
    var delta = width - s.length
    if delta < 0 then delta = 0
    s + padding.toString * delta

