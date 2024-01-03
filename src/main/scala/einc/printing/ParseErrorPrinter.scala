package einc.printing

import einc.core.Span
import einc.parserc.*

case class DescTree(desc: String, branches: DescForest, errors: List[ParseError]):
  def insertWith(key: List[String], err: ParseError): DescTree =
    key match
      case Nil => this.copy(errors = err :: errors)
      case k :: ks => this.copy(branches = branches.insertWith(k, ks, err))

  def size: Int = branches.size + errors.length

  def isChain: Boolean =
    branches.trees match
      case Nil => true
      case t :: Nil => t.isChain && errors.isEmpty
      case t :: ts => false

  def simplify: DescTree =
    branches.trees match
      case Nil => this
      case t :: Nil if errors.isEmpty => t
      case _ => this

  def simplifyUnlessChain: DescTree =
    if isChain then this else simplify

case class DescForest(trees: List[DescTree]):
  def insertWith(cur: String, rem: List[String], err: ParseError): DescForest =
    def go(ts: List[DescTree]): List[DescTree] = ts match
      case Nil => DescTree(cur, DescForest(Nil), Nil).insertWith(rem, err) :: Nil
      case t :: ts =>
        if t.desc == cur then t.insertWith(rem, err) :: ts
        else t :: go(ts)
    DescForest(go(trees))

  def insert(err: ParseError): DescForest =
    val descs = err.descs.reverse
    insertWith(descs.head, descs.tail, err)

  def size: Int = trees.map(_.size).sum

  def largestTree: DescTree = trees.maxBy(_.size)

object DescForest:
  def from(err: List[ParseError]): DescForest =
    var result = DescForest(Nil)
    err.foreach: err =>
      result = result.insert(err)
    result

class ParseErrorPrinter(source: String, err: ParseError) extends Printer:
  val selectedErrors: List[ParseError] = err.selectErrors

  val span: Span = Span(selectedErrors.head.pos.pos, 1)

  def show: String =
    val (msg, addenda) =
      if selectedErrors.exists(_.descs.nonEmpty) then
        val selected = selectedErrors.filter(_.descs.nonEmpty)
        ???
      else
        (selectedErrors.head.msg, "This is a parsing error." :: Nil)
    msg.showMessageWithSpan(span, msg, addenda)

