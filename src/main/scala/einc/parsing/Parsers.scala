package einc
package parsing

import core.*
import parserc.*
import parserc.Parsers.*
import Parser.{*, given}
import FunctorOps.*
import MonadOps.*
import ApplicativeOps.*
import AlternativeOps.*

object Parsers:
  import untpd.*

  extension [X <: Positioned](using X <:< Positioned)(px: Parser[X])
    def setPos: Parser[X] =
      (originalSource ^~ currentPos ^~ px ^~ currentPos).map: (source, start, x, finish) =>
        x.withPos(SourceSpan(source, Span.fromSourcePos(start, finish)))

  def whitespaces(atLeastOne: Boolean): Parser[Option[Int]] = Parser: input =>
    var hasLineBreak: Boolean = false
    var level: Int = 0
    var consumed: Boolean = false
    var now: ParseInput = input
    @annotation.tailrec def go(): Unit =
      now.currentChar match
        case Some(ch) if ch.isWhitespace =>
          if ch == '\n' then
            hasLineBreak = true
            level = 0
          else
            level += 1
          now = now.forward
          consumed = true
          go()
        case _ =>
    go()
    val res =
      if hasLineBreak then Some(level) else None
    if consumed || !atLeastOne then
      ParseOk(res, now, Nil)
    else
      ParseError("At least one whitespace is expected here", now.current, Nil, input.current)

  enum IndentMode:
    case StrictHigher
    case Higher
    case SameLevel
  import IndentMode.*

  def consumeTS(atLeastOne: Boolean, indentMode: IndentMode = StrictHigher): Parser[Unit] =
    (getCtx ^~ whitespaces(atLeastOne)).flatMap:
      case (ctx, Some(level)) if ctx.indentLevel >= level && indentMode == StrictHigher =>
        fail("This is misindented, a strictly higher indent level is expected")
      case (ctx, Some(level)) if ctx.indentLevel > level && indentMode == Higher =>
        fail("This is misindented, a higher indent level is expected")
      case (ctx, Some(level)) if ctx.indentLevel != level && indentMode == SameLevel =>
        fail("This is misindented, same indent level is expected")
      case _ => ().inject

  def ws: Parser[Unit] = whitespaces(atLeastOne = false) >> ().inject

  /** Whitespaces on a higher indentation level */
  def wsh: Parser[Unit] = consumeTS(atLeastOne = false, indentMode = Higher) >> ().inject

  def initWS: Parser[Unit] = consumeTS(atLeastOne = false, indentMode = SameLevel) >> ().inject

  def lineBreak: Parser[Unit] =
    (getCtx ^~ whitespaces(atLeastOne = false)).flatMap: (ctx, mlevel) =>
      mlevel match
        case Some(level) if level == ctx.indentLevel => ().inject
        case _ => fail(s"Not a line break at level ${ctx.indentLevel}")

  extension [X](px: Parser[X])
    def inParens: Parser[X] =
      px.tsSame.surroundedBy(string("(").ts.withDesc("`(`"), string(")").withDesc("`)`"))

    def inBrackets: Parser[X] =
      px.tsSame.surroundedBy(keyword("[").ts, keyword("]"))

    def inBraces: Parser[X] =
      px.tsSame.surroundedBy(keyword("{").ts, keyword("}"))

    def trailingSpaces: Parser[X] = px << consumeTS(atLeastOne = false).dropDesc

    def trailingSpaces1: Parser[X] = px << consumeTS(atLeastOne = true).dropDesc

    def ts: Parser[X] = trailingSpaces

    def ts1: Parser[X] = trailingSpaces1

    def tsSame: Parser[X] = px << consumeTS(atLeastOne = false, indentMode = Higher).dropDesc

    def indented(newLevel: Int): Parser[X] = Parser: input =>
      px.runParser(input)(using ctx.withIndentLevel(newLevel))

  val KEYWORD_LIST = Set(
    "val",
    "def",
    "match",
    "with"
  )

  val nameP: Parser[String] =
    val p =
      (alpha.withDesc("an alphabetic character").withDesc("start of an identifier") ^~ whileP(ch => ch.isLetterOrDigit || ch == '-').withDesc("the remaining of an identifier")).map: (x, xs) =>
        x.toString + xs
    p.flatMap: name =>
      if KEYWORD_LIST.contains(name) then
        fail(s"Invalid identifier name: `$name` is a keyword")
      else name.inject

  def keyword(text: String): Parser[Unit] =
    string(text).withDesc(s"`$text`")

  val numberP: Parser[String] =
    digit.withDesc("a digit").some.map(_.mkString)

  val stringLitP: Parser[String] =
    val p = whileP(_ != '"', toAvoid = _ == '\n')
    val delimiter = char('"').withDesc("`\"`")
    p.surroundedBy(delimiter, delimiter)

  /** Parser for notation rules */
  object notationRule:
    /** Parses `ident:prec` in the pattern */
    val identP: Parser[NotationPatternPart.Ident] =
      (nameP.withDesc("name of the identifier") ^~ keyword(":") ^~ numberP.withDesc("precedence")).map: (ident, _, prec) =>
        NotationPatternPart.Ident(ident, prec.toInt)

    /** Parses operators in the pattern */
    val operatorP: Parser[NotationPatternPart.Operator] =
      stringLitP.map(NotationPatternPart.Operator(_))

    /** Parses the pattern */
    val patternP: Parser[NotationPattern] =
      val p = identP.withDesc("a pattern identifier") <|> operatorP.withDesc("an operator")
      p.trailingSpaces.some.withDesc("a notation pattern").map(NotationPattern(_))

    /** Parses the part `notation:prec` */
    val keywordP: Parser[Int] =
      (keyword("notation") >> keyword(":") >> numberP.withDesc("precedence")).map(_.toInt)

    val ruleP: Parser[NotationRule] =
      (keywordP.withDesc("the `notation` keyword").trailingSpaces1
        ^~ patternP.withDesc("a notation pattern").trailingSpaces
        ^~ keyword("=>").trailingSpaces
        ^~ expression.parser.withDesc("rhs of the notation rule")).map: (prec, pattern, _, rhs) =>
          NotationRule(prec, pattern, rhs)

    val parser: Parser[NotationRule] = ruleP.withDesc("a notation rule")

  object typeExpr:
    import TypeExpr.*

    val typeRefP: Parser[TypeRef] =
      nameP.withDesc("name of a type").map(TypeRef(_))

    val appliedTypeP: Parser[TypeExpr] =
      def params: Parser[List[TypeExpr]] =
        parser.sepBy1(consumeTS(atLeastOne = false, indentMode = IndentMode.Higher) >> keyword(",").ts).inBrackets

      (typeRefP.setPos ^~ params.optional).map: (head, args) =>
        args match
          case Some(args) => AppliedType(head, args)
          case None => head

    val funTypeP: Parser[TypeExpr] =
      val baseP = appliedTypeP <|> parser.inParens
      def paramListP =
        parser.sepBy(wsh >> keyword(",") << wsh).inParens
      def paramsP =
        baseP.map(List(_)) <|> paramListP
      (paramsP.ts ^~ keyword("=>").ts ^~ parser).map: (argTypes, _, resType) =>
        FunctionType(argTypes, resType)

    val parser: Parser[TypeExpr] =
      val p = funTypeP <|> appliedTypeP
      p.setPos

  def buildArrowLikeParser[X <: Positioned](base: Parser[X], cons: (X, X) => X, resultDesc: String): Parser[X] =
    val imp: Parser[X] = (consumeTS(atLeastOne = false) >> keyword("=>").ts >> base.withDesc(resultDesc))
    val imps: Parser[List[X]] = imp.many
    @annotation.tailrec def recur(head: X, xs: List[X], cont: X => X): X = xs match
      case Nil => cont(head)
      case x :: xs =>
        recur(x, xs, r =>
          cont(cons(head, r).withPos(head.pos -- r.pos)))
    val p =
      (base ^~ imps).map: (head, imps) =>
        recur(head, imps, r => r)
    p.setPos

  object typeKind:
    import tpd.TypeKind
    import TypeKind.*

    /** Parser for `Type` */
    val starP: Parser[TypeKind] = keyword("Type") #> Star

    /** Parser for `(...)` */
    val baseP: Parser[TypeKind] =
      starP.setPos <|> parser.inParens

    /** Parser for type kinds */
    val parser: Parser[TypeKind] =
      buildArrowLikeParser(baseP, (head, r) => Arrow(head, r).withPos(head.pos -- r.pos), "result kind").withDesc("type kind")

  object definition:
    import Definition.*

    val valDefP: Parser[ValDef] =
      val p =
        (keyword("val").ts ^~ nameP.ts.withDesc("binding name") ^~ keyword("=") ^~ expression.maybeBlockParser).map: (_, name, _, body) =>
          ValDef(name, body)
      p.setPos.withDesc("a `val` definition")

    val defDefP: Parser[DefDef] =
      def typeParams: Parser[List[DefTypeParam]] =
        nameP.map(DefTypeParam(_)).setPos.sepBy(wsh >> keyword(",") << wsh).inBrackets
      def synParams: Parser[List[DefSynthesisParam]] =
        (nameP.ts ^~ keyword(":").ts ^~ typeExpr.parser)
          .map((name, _, tpe) => DefSynthesisParam(name, tpe)).setPos
          .sepBy(wsh >> keyword(",") << wsh)
          .inBraces
      def termParams: Parser[List[DefTermParam]] =
        (nameP.ts ^~ keyword(":").ts ^~ typeExpr.parser)
          .map((name, _, tpe) => DefTermParam(name, tpe)).setPos
          .sepBy(wsh >> keyword(",") << wsh)
          .inParens
      def paramList: Parser[DefParamList] =
        typeParams.map(TypeParamList(_)).withDesc("a type parameter list")
          <|> synParams.map(SynthesisParamList(_)).withDesc("a synthetic parameter list")
          <|> termParams.map(TermParamList(_)).withDesc("a term parameter list")
      val resType: Parser[TypeExpr] =
        keyword(":").ts >> typeExpr.parser
      val p =
        (keyword("def").ts >>
          nameP.withDesc("binding name").ts ^~
          paramList.ts.many ^~
          resType.ts.optional.withDesc("return type") ^~
          keyword("=") ^~
          expression.maybeBlockParser.withDesc("body")).map: (name, paramss, resType, _, body) =>
            DefDef(name, paramss, resType, body)
      p.setPos.withDesc("a `def` definition")

    def parser: Parser[Definition] = localParser

    def localParser: Parser[LocalDef] = valDefP <|> defDefP

  object expression:
    import ExprParser.*
    import Expr.*

    val identP: Parser[Ident] = nameP.withDesc("an identifier").map(Ident(_))

    val intLitP: Parser[IntLit] = numberP.withDesc("a number literal").map(x => IntLit(x.toInt))

    val strLitP: Parser[StringLit] = stringLitP.withDesc("a string literal").map(StringLit(_))

    def parser: Parser[Expr] = eincExprParser.exprP

    def maybeBlockParser: Parser[Expr] = eincExprParser.maybeBlockP

    object eincExprParser extends ExprParser:
      val baseP: Parser[Expr] =
        identP
          <|> intLitP
          <|> strLitP
          <|> exprP.ts.surroundedBy(string("(").ts.withDesc("`(`"), string(")").withDesc("`)`"))

      val applyP: Parser[Expr] =
        val args = exprP.tsSame.sepBy(string(",").ts.withDesc("`,`"))
        val argList = currentPos ^~ args.withDesc("function arguments").inParens ^~ currentPos
        val argLists = argList.many
        @annotation.tailrec def recur(source: String, head: Expr, argss: List[(SourcePos, List[Expr], SourcePos)]): Expr = argss match
          case Nil => head
          case (start, args, finish) :: argss => recur(source, Apply(head, args).withPos(SourceSpan(source, Span.fromSourcePos(start, finish))), argss)
        (originalSource ^~ baseP ^~ argLists).map: (source, head, argss) =>
          recur(source, head, argss)
      val lambdaP: Parser[Expr] =
        val singleParamP: Parser[List[LambdaParam]] =
          nameP.withDesc("parameter name").map(LambdaParam(_, None)).setPos.map(List(_))
        val paramP: Parser[LambdaParam] =
          val p =
            (nameP.withDesc("parameter name").ts ^~ (string(":").ts.withDesc("`:`") >> typeExpr.parser.withDesc("parameter type")).optional).map: (name, typ) =>
              LambdaParam(name, typ)
          p.setPos
        val normalParamListP: Parser[List[LambdaParam]] =
          paramP.ts.sepBy(string(",").ts.withDesc("`,`")).inParens
        val paramListP = singleParamP <|> normalParamListP
        val p =
          (paramListP.withDesc("the parameter list of a lambda").ts ^~ string("=>").withDesc("`=>`") ^~ maybeBlockP.withDesc("the body of a lambda")).map: (params, _, body) =>
            Lambda(params, body)
        p

      def maybeBlockP: Parser[Expr] =
        whitespaces(atLeastOne = false).flatMap:
          case None => exprP
          case Some(level) => getCtx.flatMap: ctx =>
            if level <= ctx.indentLevel then
              fail("Block is not right-indented")
            else
              val localDefs: Parser[List[LocalDef]] =
                (definition.localParser << lineBreak).many
              val p = (localDefs ^~ exprP).map: (localDefs, expr) =>
                Block(localDefs, expr)
              p.indented(level)

      def atom: Parser[Expr] = lambdaP.withDesc("a lambda") <|> applyP

      def exprP: Parser[Expr] = getParser(Precedence.MIN).setPos
