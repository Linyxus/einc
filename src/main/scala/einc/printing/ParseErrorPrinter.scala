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

  def describe(err: ParseError): Option[(String, String)] =
    val res =
      err.descs match
        case Nil => None
        case x :: Nil => Some(x)
        case x1 :: x2 :: Nil => Some(s"$x1 when parsing $x2")
        case x1 :: x2 :: x3 :: _ => Some(s"$x1 as part of $x2 when parsing $x3")
    res.map((err.msg, _))

  def show: String =
    val (msg, addenda) =
      val descStrs = selectedErrors.flatMap(describe)
      descStrs match
        case Nil =>
          (selectedErrors.head.msg, "This is a parsing error." :: Nil)
        case (msg, x) :: Nil =>
          (msg, s"This is a parsing error. Expecting $x" :: Nil)
        case xs =>
          val strs = xs.map(x => s" - ${x._2}")
          ("Unexpected input\n", s"This is a parsing error. Expecting one of the following:\n" :: strs)
    source.showMessageWithSpan(span, msg, addenda)

extension (err: ParseError)
  def show(source: String): String =
    val printer = ParseErrorPrinter(source, err)
    val str: String = printer.show
    str

