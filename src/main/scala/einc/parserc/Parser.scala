package einc.parserc

case class SourcePos(idx: Int):
  def next: SourcePos = SourcePos(idx + 1)

case class ParseError(pos: SourcePos, msg: String, isRecoverable: Boolean = false):
  def toRecoverable: ParseError = copy(isRecoverable = true)
  def toFatal: ParseError = copy(isRecoverable = false)

/** A parser combinator. */
trait Parser[+X]:
  def run(input: InputState): Either[ParseError, (InputState, X)]

  def mapError(f: ParseError => ParseError): Parser[X] = Parser: input =>
    run(input) match
      case Left(err) => Left(f(err))
      case Right(r) => Right(r)

  def withError(err: ParseError): Parser[X] = mapError(_ => err)

  def recoverable: Parser[X] = mapError(_.toRecoverable)

  def fatal: Parser[X] = mapError(_.toFatal)

object Parser:
  def apply[X](f: InputState => Either[ParseError, (InputState, X)]) = new Parser[X]:
    def run(input: InputState): Either[ParseError, (InputState, X)] = f(input)

  def fail(msg: String, isRecoverable: Boolean = false): Parser[Nothing] = Parser: input =>
    Left(ParseError(input.pos, msg, isRecoverable))

  def flatten[A](ppa: Parser[Parser[A]]): Parser[A] = Parser: input =>
    ppa.run(input).flatMap: (input1, pa) =>
      pa.run(input1)

  given parserIsApplicative: Applicative[Parser] with
    def pure[X](x: X): Parser[X] = Parser: input =>
      Right(input, x)

    extension [A](fa: Parser[A])
      def map[B](op: A => B): Parser[B] = Parser: input =>
        fa.run(input) match
          case Left(err) => Left(err)
          case Right((o, x)) => Right((o, op(x)))

    extension [A, B](ff: Parser[A => B])
      def <*>(fa: => Parser[A]): Parser[B] = Parser: input =>
        ff.run(input).flatMap: (input1, f) =>
          fa.run(input1).map: (input2, x) =>
            (input2, f(x))

  given parserIsMonad: Monad[Parser] with
    def unit[X](x: X): Parser[X] = x.embed

    extension [A](fa: Parser[A])
      def flatMap[B](op: A => Parser[B]): Parser[B] = flatten(op <#> fa)

  given parserIsAlternative: Alternative[Parser] with
    def fail[X]: Parser[X] = Parser.fail("None of the alternatives matched")

    extension [A](fa: Parser[A]) 
      def <|>[B](fb: => Parser[B]): Parser[A | B] = Parser: input =>
        fa.run(input) match
          case Left(err) =>
            if err.isRecoverable then fb.run(input)
            else Left(err)
          case Right((o, x)) => Right((o, x))
