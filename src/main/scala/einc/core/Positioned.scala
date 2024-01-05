package einc.core

trait Positioned:
  private var myPos: SourceSpan | Null = null

  def pos: SourceSpan = myPos.nn

  def hasPos: Boolean = myPos ne null

  def setPos(span: SourceSpan): Unit =
    myPos = span

  def withPos(span: SourceSpan): this.type =
    setPos(span)
    this

