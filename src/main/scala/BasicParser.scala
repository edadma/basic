package xyz.hyperreal.basic

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{CharSequenceReader, Position, Positional}

object BasicParser {

  def parseProgram(line: String, parser: BasicParser): List[LineAST] = {
    parser.parseFromString(line, parser.program)
  }

}

class BasicParser extends RegexParsers {

  override protected val whiteSpace: Regex = """[ \t\r]+""".r

  def pos: Parser[Position] = positioned(success(new Positional {})) ^^ {
    _.pos
  }

  def line: Parser[Int] = """\d+""".r ^^ (s => s.toInt)

  def number: Parser[NumberAST] =
    positioned("""-?\d+(\.\d*)?""".r ^^ (s => NumberAST(s.toDouble)))

  def string: Parser[StringAST] =
    positioned(""""(?:""|[^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""".r ^^ (s =>
      StringAST(s.substring(1, s.length - 1))))

  def ident: Parser[String] = """[a-zA-Z_$%][a-zA-Z0-9_$%]*""".r ^^ (_.toUpperCase)

  def variable: Parser[VariableAST] =
    positioned(ident ~ opt("[" ~> expression <~ "]") ^^ {
      case name ~ sub => VariableAST(name, sub)
    })

  def program: Parser[List[LineAST]] = opt("""\s+""" r) ~> repsep(programLine, """\s+""" r) <~ opt("""\s+""" r)

  def programLine: Parser[LineAST] =
    positioned(line ~ statement ~ opt("'.*" r) ^^ {
      case l ~ s ~ c => LineAST(l, s, c)
    })

  def statement: Parser[StatementAST] =
    positioned(
      ("print" | "PRINT") ~> rep(expression ~ opt(";" | ",") ^^ { case e ~ s => (e, s) }) ^^ PrintAST |
        opt("let" | "LET") ~> (variable ~ "=" ~ expression) ^^ {
          case v ~ _ ~ e => LetAST(v, e)
        } |
        opt("goto" | "GOTO") ~> line ^^ GotoAST |
        opt("end" | "END") ^^ (_ => EndAST())
    )

  def expression: Parser[ExpressionAST] =
    additive

  def additive: Parser[ExpressionAST] = multiplicative ~ rep(pos ~ ("+" | "-") ~ multiplicative) ^^ {
    case e ~ l =>
      l.foldLeft(e) {
        case (x, p ~ o ~ y) => InfixAST(x, p, o, y)
      }
  }

  def multiplicative: Parser[ExpressionAST] = primary ~ rep(pos ~ ("*" | "/") ~ primary) ^^ {
    case e ~ l =>
      l.foldLeft(e) {
        case (x, p ~ o ~ y) => InfixAST(x, p, o, y)
      }
  }

  def function: Parser[FunctionAST] =
    positioned(ident ~ "(" ~ rep1(expression) ~ ")" ^^ {
      case name ~ _ ~ args ~ _ => FunctionAST(name, args)
    })

  def primary: Parser[ExpressionAST] =
    "-" ~> expression ^^ (PrefixAST("-", _)) |
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
}
