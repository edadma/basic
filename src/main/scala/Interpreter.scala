package xyz.hyperreal.basic

import scala.collection.mutable

class Interpreter {

  private val WIDTH = 55
  private val lines = mutable.SortedMap.empty[Int, LineAST]
  private val vars = mutable.HashMap.empty[String, Any]
  private val functions =
    Map(
      "SQRT" -> ((args: List[Any]) => math.sqrt(args.head.asInstanceOf[Double]))
    )
  private var loc = Iterator.empty[(Int, LineAST)]
  private val precedences = Map("+" -> 1, "-" -> 1, "*" -> 2, "/" -> 2)
  private val spaces = Map("+" -> true, "-" -> true, "*" -> false, "/" -> false)

  def load(program: List[LineAST]): Unit = program foreach add

  def add(line: LineAST): Unit =
    line match {
      case l @ LineAST(0, _, _) => problem(l.pos, "zero is not a valid line number")
      case l @ LineAST(n, _, _) =>
        if (lines contains n)
          problem(l.pos, s"line number $n has already been encountered")
        else
          lines(n) = line
    }

  def list(from: Option[Int], to: Option[Int]): Unit = {
    def expression(expr: ExpressionAST, prec: Int = 0): String =
      expr match {
        case NumberAST(n)   => if (n.isWhole) n.toLong.toString else n.toString
        case StringAST(s)   => s""""$s""""
        case VariableAST(v) => v
        case InfixAST(left, _, op, right) =>
          val p = precedences(op)
          val s = if (spaces(op)) " " else ""
          val l = expression(left, p)
          val r = expression(right, p)
          val (lp, rp) = if (prec > p) ("(", ")") else ("", "")

          s"$lp$l$s$op$s$r$rp"
      }

    if (lines.nonEmpty) {
      val start =
        from match {
          case None    => lines.firstKey
          case Some(n) => n
        }
      val end =
        to match {
          case None    => lines.lastKey
          case Some(n) => n
        }
      val range = lines.rangeFrom(start).rangeTo(end)
      val width = range.lastKey.toString.length

      range foreach {
        case (line, LineAST(_, stat, comm)) =>
          val s =
            stat match {
              case EndAST()                        => "END"
              case NopAST()                        => ""
              case LetAST(VariableAST(name), expr) => s"LET $name = ${expression(expr)}"
              case PrintAST(args) =>
                s"PRINT ${args map { case (e, sep) => s"${expression(e)}${sep getOrElse ""}" } mkString " "}"
            }
          val l = line.formatted(s"%${width}s")
          val c = s"$l $s"
          val p =
            if (c.length > WIDTH) " "
            else " " * (WIDTH - c.length)

          println(s"$c$p${comm getOrElse ""}")
      }
    }
  }

  def run(): Unit = {
    loc = lines.iterator

    while (loc != null && loc.hasNext) {
      loc.next match {
        case (_, l @ LineAST(_, stat, _)) => perform(stat)
      }
    }
  }

  def perform(stat: StatementAST): Unit =
    stat match {
      case NopAST() | RemAST(_) =>
      case PrintAST(args) =>
        args foreach {
          case (expr, None | Some(",")) => println(show(expr))
          case (expr, Some(";"))        => print(show(expr))
        }
      case LetAST(v @ VariableAST(name), expr) =>
        if (functions contains name)
          problem(v.pos, s"reserved function name")
        else
          vars(name) = eval(expr)
      case GotoAST(line) => loc = lines.iteratorFrom(line)
      case EndAST()      => loc = null
    }

  def show(a: ExpressionAST): String =
    eval(a) match {
      case w: Double if w.isWhole => w.toLong.toString
      case v                      => String.valueOf(v)
    }

  def eval(expr: ExpressionAST): Any =
    expr match {
      case NumberAST(n) => n
      case StringAST(s) => s
      case e @ VariableAST(name) =>
        vars get name match {
          case None    => problem(e.pos, s"variable '$name' not found")
          case Some(v) => v
        }
      case InfixAST(left, oppos, op, right) =>
        val l = eval(left)
        val r = eval(right)

        (l, op, r) match {
          case (a: Double, "+", b: Double) => a + b
          case (_, "+", _)                 => problem(oppos, s"can't add '$l' and '$r'")
          case (a: Double, "*", b: Double) => a * b
          case (_, "*", _)                 => problem(oppos, s"can't multiply '$l' and '$r'")
          case _                           => problem(oppos, s"operation '$op' unrecognized")
        }
    }

}
