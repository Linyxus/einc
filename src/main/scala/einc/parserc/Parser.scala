package einc.parserc

case class SourcePos(idx: Int):
  def next: SourcePos = SourcePos(idx + 1)

case class ParseError(pos: SourcePos, msg: String, isRecoverable: Boolean = false):
  def toRecoverable: ParseError = copy(isRecoverable = true)
  def toFatal: ParseError = copy(isRecoverable = false)

type ParseResult[X] = ParseError | (InputState, X)

/** A parser combinator. */
trait Parser[+X]:
  def run(input: InputState): ParseResult[X]

  def mapError(f: ParseError => ParseError): Parser[X] = Parser: input =>
    run(input) match
      case err: ParseError => f(err)
      case (input1, x) => (input1, x)

  def withError(err: ParseError): Parser[X] = mapError(_ => err)

  def recoverable: Parser[X] = mapError(_.toRecoverable)

  def fatal: Parser[X] = mapError(_.toFatal)

object Parser:
  def apply[X](f: InputState => ParseResult[X]) = new Parser[X]:
    def run(input: InputState): ParseResult[X] = f(input)

  def fail(msg: String, isRecoverable: Boolean = false): Parser[Nothing] = Parser: input =>
    ParseError(input.pos, msg, isRecoverable)

  def flatten[A](ppa: Parser[Parser[A]]): Parser[A] = Parser: input =>
    ppa.run(input) match
      case (input1, pa) =>
        pa.run(input1)
      case err: ParseError => err

  given parserIsApplicative: Applicative[Parser] with
    def pure[X](x: X): Parser[X] = Parser: input =>
      (input, x)

    extension [A](fa: Parser[A])
      def map[B](op: A => B): Parser[B] = Parser: input =>
        fa.run(input) match
          case err: ParseError => err
          case (o, x) => (o, op(x))

    extension [A, B](ff: Parser[A => B])
      def <*>(fa: => Parser[A]): Parser[B] = Parser: input =>
        ff.run(input) match 
          case (input1, f) =>
            fa.run(input1) match 
              case (input2, x) => (input2, f(x))
              case err: ParseError => err
          case err: ParseError => err

  given parserIsMonad: Monad[Parser] with
    def unit[X](x: X): Parser[X] = x.embed

    extension [A](fa: Parser[A])
      def flatMap[B](op: A => Parser[B]): Parser[B] = flatten(op <#> fa)

  given parserIsAlternative: Alternative[Parser] with
    def fail[X]: Parser[X] = Parser.fail("None of the alternatives matched")

    extension [A](fa: Parser[A]) 
      def <|>[B](fb: => Parser[B]): Parser[A | B] = Parser: input =>
        fa.run(input) match
          case err: ParseError =>
            if err.isRecoverable then fb.run(input)
            else err
          case x => x

  extension [X](res: ParseResult[X])
    def getError: ParseError = res match
      case err: ParseError => err
      case _ => assert(false, "ParseResult is not an error")

    def getResult: X = res match
      case (_, x) => x
      case _ => assert(false, "ParseResult is an error")

    def getInputState: InputState = res match
      case (input, _) => input
      case _ => assert(false, "ParseResult is an error")

    def isOk: Boolean = res match
      case _: ParseError => false
      case _ => true

    def isError: Boolean = !isOk

  extension (s: String)
    def parseWith[X](p: Parser[X]): ParseResult[X] =
      p.run(InputState.fromString(s))
