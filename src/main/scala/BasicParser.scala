package xyz.hyperreal.basic

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{CharSequenceReader, Position, Positional}

class BasicParser extends RegexParsers {

  override protected val whiteSpace: Regex = """[ \t\r]+""".r

  def pos: Parser[Position] = positioned(success(new Positional {})) ^^ {
    _.pos
  }

  def integer: Parser[Int] = """\d+""".r ^^ (s => s.toInt)

  def number: Parser[NumberAST] =
    positioned("""-?\d+(\.\d*)?""".r ^^ (s => NumberAST(s.toDouble)))

  def boolean: Parser[BooleanAST] =
    positioned((kw("true") | kw("false")) ^^ (s => BooleanAST(s.toUpperCase == "TRUE")))

  def string: Parser[StringAST] =
    positioned(""""(?:""|[^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r ^^ (s =>
      StringAST(s.substring(1, s.length - 1))))

  def ident: Parser[String] = """[a-zA-Z_$%][a-zA-Z0-9_$%]*""".r ^^ (_.toUpperCase)

  def variable: Parser[VariableAST] =
    positioned(ident ~ opt("[" ~> expression <~ "]" | "(" ~> expression <~ ")") ^^ {
      case name ~ sub => VariableAST(name, sub)
    })

  def program: Parser[List[LineAST]] = opt("""\s+""" r) ~> repsep(programLine, """\s+""" r) <~ opt("""\s+""" r)

  def programLine: Parser[LineAST] =
    positioned(integer ~ statement ~ opt("'.*" r) ^^ {
      case l ~ s ~ c => LineAST(l, s, c)
    })

  def kw(s: String): Regex = s"(${s.toLowerCase}|${s.toUpperCase})\\b".r

  def statement: Parser[StatementAST] =
    positioned(
      kw("for") ~ variable ~ "=" ~ expression ~ kw("to") ~ expression ~ opt(kw("step") ~> expression) ^^ {
        case _ ~ v ~ _ ~ f ~ _ ~ t ~ s => ForAST(v, f, t, s)
      } |
        kw("next") ~ variable ^^ {
          case _ ~ v => NextAST(v)
        } |
        kw("if") ~ expression ~ kw("then") ~ (statement | (integer ^^ GotoAST)) ~ opt(
          kw("else") ~> (statement | (integer ^^ GotoAST))) ^^ {
          case _ ~ c ~ _ ~ y ~ n => IfAST(c, y, n)
        } |
        kw("dim") ~> (ident ~ ("[" ~> integer <~ "]" | "(" ~> integer <~ ")")) ^^ {
          case n ~ d => DimAST(n, d)
        } |
        kw("print") ~> rep(expression ~ opt(";" | ",") ^^ { case e ~ s => (e, s) }) ^^ PrintAST |
        opt(kw("let")) ~> (variable ~ "=" ~ expression) ^^ {
          case v ~ _ ~ e => LetAST(v, e)
        } |
        kw("goto") ~> integer ^^ GotoAST |
        kw("end") ^^ (_ => EndAST())
    )

  val leftAssociative: (ExpressionAST ~ List[Position ~ String ~ ExpressionAST]) => ExpressionAST = {
    case e ~ l =>
      l.foldLeft(e) {
        case (x, p ~ o ~ y) => InfixAST(x, p, o.toUpperCase, y)
      }
  }

  def expression: Parser[ExpressionAST] =
    orExpression

  def orExpression: Parser[ExpressionAST] =
    andExpression ~ rep(pos ~ kw("or") ~ andExpression) ^^ leftAssociative

  def andExpression: Parser[ExpressionAST] =
    notExpression ~ rep(pos ~ kw("and") ~ notExpression) ^^ leftAssociative

  def notExpression: Parser[ExpressionAST] = opt(kw("not")) ~ comparisonExpression ^^ {
    case None ~ c => c
    case _ ~ c    => PrefixAST("NOT", c)
  }

  def comparisonExpression: Parser[ExpressionAST] =
    additive ~ opt(pos ~ ("<=" | ">=" | "<>" | "<" | ">" | "=") ~ additive) ^^ {
      case a ~ None            => a
      case l ~ Some(p ~ o ~ r) => InfixAST(l, p, o, r)
    }

  def additive: Parser[ExpressionAST] = multiplicative ~ rep(pos ~ ("+" | "-") ~ multiplicative) ^^ leftAssociative

  def multiplicative: Parser[ExpressionAST] = primary ~ rep(pos ~ ("*" | "/") ~ primary) ^^ leftAssociative

  def function: Parser[FunctionAST] =
    positioned(ident ~ "(" ~ rep1(expression) ~ ")" ^^ {
      case name ~ _ ~ args ~ _ => FunctionAST(name, args)
    })

  def primary: Parser[ExpressionAST] =
    "-" ~> expression ^^ (PrefixAST("-", _)) |
      boolean |
      number |
      string |
      function |
      variable |
      "(" ~> expression <~ ")"

  def parseFromString[T](src: String, grammar: Parser[T]): T =
    parseAll(grammar, new CharSequenceReader(src)) match {
      case Success(tree, _)       => tree
      case NoSuccess(error, rest) => problem(rest.pos, error)
    }

  def parseProgram(code: String): List[LineAST] = parseFromString(code, program)

}
