package einc.core

case class SourceSpan(source: String, span: Span):
  def -- (other: SourceSpan): SourceSpan =
    copy(span = span -- other.span)

