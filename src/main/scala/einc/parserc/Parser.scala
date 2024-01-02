package einc.parserc

case class SourcePos(idx: Int):
  assert(idx >= -1)
  def next: SourcePos = 
    if isNoPos then
      this
    else
      SourcePos(idx + 1)

  def isNoPos: Boolean = idx == -1

  def <=(that: SourcePos): Boolean = idx <= that.idx

case class ParseError(pos: SourcePos, msg: String, expected: List[String] = Nil):
  def <|>(that: ParseError): ParseError =
    if that.pos <= pos then
      this
    else
      that

case class ParseOutcome[+X](nextInput: InputState, errors: List[ParseError], result: X):
  def >>[Y](res: ParseResult[Y]): ParseResult[Y] = res match
    case err: ParseError => err
    case ParseOutcome(nextInput1, errors1, result1) =>
      ParseOutcome(nextInput1, errors ++ errors1, result1)

type ParseResult[X] = ParseError | ParseOutcome[X]

/** A parser combinator. */
trait Parser[+X]:
  def run(input: InputState): ParseResult[X]

  def mapError(f: ParseError => ParseError): Parser[X] = Parser: input =>
    run(input) match
      case err: ParseError => f(err)
      case outcome => outcome

  def withError(err: ParseError): Parser[X] = mapError(_ => err)

  def describe(msg: String): Parser[X] = mapError:
    case ParseError(pos, e, expected) => ParseError(pos, e, msg :: expected)

object Parser:
  def apply[X](f: InputState => ParseResult[X]) = new Parser[X]:
    def run(input: InputState): ParseResult[X] = f(input)

  def fail(msg: String): Parser[Nothing] = Parser: input =>
    ParseError(input.pos, msg)

  def flatten[A](ppa: Parser[Parser[A]]): Parser[A] = Parser: input =>
    ppa.run(input) match
      case o @ ParseOutcome(input1, errs, pa) =>
        o >> pa.run(input1)
      case err: ParseError => err

  given parserIsApplicative: Applicative[Parser] with
    def pure[X](x: X): Parser[X] = Parser: input =>
      ParseOutcome(input, Nil, x)

    extension [A](fa: Parser[A])
      def map[B](op: A => B): Parser[B] = Parser: input =>
        fa.run(input) match
          case err: ParseError => err
          case ParseOutcome(o, e, x) => ParseOutcome(o, e, op(x))

    extension [A, B](ff: Parser[A => B])
      def <*>(fa: => Parser[A]): Parser[B] = Parser: input =>
        ff.run(input) match 
          case ParseOutcome(input1, errs1, f) =>
            fa.run(input1) match 
              case ParseOutcome(input2, errs2, x) => ParseOutcome(input2, errs1 ++ errs2, f(x))
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
          case err: ParseError => fb.run(input) match
            case err1: ParseError => err <|> err1
            case ParseOutcome(n, e, r) => ParseOutcome(n, err :: e, r)
          case res => res

  extension [X](res: ParseResult[X])
    def getError: ParseError = res match
      case err: ParseError => err
      case _ => assert(false, "ParseResult is not an error")

    def getResult: X = res match
      case ParseOutcome(_, _, x) => x
      case _ => assert(false, "ParseResult is an error")

    def getInputState: InputState = res match
      case ParseOutcome(input, _, _) => input
      case _ => assert(false, "ParseResult is an error")

    def isOk: Boolean = res match
      case _: ParseError => false
      case _ => true

    def isError: Boolean = !isOk

  extension (s: String)
    def parseWith[X](p: Parser[X]): ParseResult[X] =
      p.run(InputState.fromString(s))
