package xyz.hyperreal.basic

import java.io.PrintStream

import scala.collection.mutable
import math._

class Interpreter(out: PrintStream = Console.out) {

  private val lines = mutable.SortedMap.empty[Int, LineAST]
  private val vars = mutable.HashMap.empty[String, Any]
  private var loc = Iterator.empty[(Int, LineAST)]
  private val precedences =
    Map("AND" -> 1, "OR" -> 2, "<" -> 10, ">" -> 10, "=" -> 10, "+" -> 20, "-" -> 20, "*" -> 30, "/" -> 30)
  private val spaces =
    Map("AND" -> true,
        "OR" -> true,
        "<" -> true,
        ">" -> true,
        "=" -> true,
        "+" -> true,
        "-" -> true,
        "*" -> false,
        "/" -> false)

  List[(String, Double => Double)](
    "SQR" -> sqrt,
    "SGN" -> signum,
    "INT" -> floor,
    "ABS" -> abs,
    "COS" -> cos,
    "SIN" -> sin,
    "TAN" -> tan,
    "EXP" -> exp,
    "LOG" -> log,
    "FLOOR" -> floor,
    "CEIL" -> ceil,
  ) foreach {
    case (n, f) => vars(n) = new BuiltinNumeric(n, f)
  }

  List(
    "PI" -> Pi,
    "E" -> E
  ) foreach {
    case (k, v) => vars(k) = Constant(v)
  }

  def load(program: List[LineAST]): Unit = program foreach add

  def add(line: LineAST): Unit =
    line match {
      case l @ LineAST(0, _, _) => problem(l.pos, "zero is not a valid line number")
      case l @ LineAST(n, _, _) =>
        if (lines contains n) problem(l.pos, s"line number $n has already been encountered")
        else lines(n) = line
    }

  def clear(): Unit = vars.clear()

  def list(from: Option[Int], to: Option[Int]): Unit = {
    def expression(expr: ExpressionAST, prec: Int = 0): String =
      expr match {
        case BooleanAST(b)           => if (b) "TRUE" else "FALSE"
        case NumberAST(n)            => if (n.isWhole) n.toLong.toString else n.toString
        case StringAST(s)            => s""""$s""""
        case VariableAST(v, None)    => v
        case VariableAST(v, Some(s)) => s"$v[${expression(s)}]"
        case InfixAST(left, _, op, right) =>
          val p = precedences(op)
          val s = if (spaces(op)) " " else ""
          val l = expression(left, p)
          val r = expression(right, p)
          val (lp, rp) = if (prec > p) ("(", ")") else ("", "")

          s"$lp$l$s$op$s$r$rp"
        case PrefixAST(op, expr)     => s"$op${if (op.head.isLetter) " " else ""}${expression(expr, prec)}"
        case FunctionAST(name, args) => s"$name(${args map (expression(_)) mkString ", "})"
      }

    def statement(stat: StatementAST): String =
      stat match {
        case ForAST(name, from, to, step) =>
          s"FOR ${expression(name)} = ${expression(from)} TO ${expression(to)}${if (step.isDefined)
            s" STEP ${expression(step.get)}"
          else ""}"
        case NextAST(name)          => s"NEXT ${expression(name)}"
        case GotoAST(line)          => s"GOTO $line"
        case DimAST(name, dim)      => s"DIM $name[$dim]"
        case EndAST()               => "END"
        case NopAST()               => ""
        case LetAST(variable, expr) => s"LET ${expression(variable)} = ${expression(expr)}"
        case PrintAST(args) =>
          s"PRINT ${args map { case (e, sep) => s"${expression(e)}${sep getOrElse ""}" } mkString " "}"
        case IfAST(cond, thenPart, elsePart) =>
          s"IF ${expression(cond)} THEN ${statement(thenPart)}${if (elsePart.isDefined) s" ELSE ${statement(elsePart.get)}"
          else ""}"
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
          val l = line.formatted(s"%${width}s")
          val s = statement(stat)

          out.println(s"$l $s${if (comm.isDefined) comm.get else ""}")
      }
    }
  }

  def run(from: Option[Int]): Unit = {
    loc = lines.iteratorFrom(from.getOrElse(1))

    try {
      while (loc != null && loc.hasNext) {
        loc.next() match {
          case (_, l @ LineAST(line, stat, _)) =>
            perform(line, stat)
        }
      }
    } catch {
      case e: Exception =>
        out.println
        out.println(e.getMessage)
    }
  }

  def evaln(expr: ExpressionAST): Double =
    eval(expr) match {
      case n: Double => n
      case _         => problem(expr.pos, "expected a number")
    }

  def evalb(expr: ExpressionAST): Boolean =
    eval(expr) match {
      case b: Boolean => b
      case _          => problem(expr.pos, "expected a boolean")
    }

  def perform(line: Int, stat: StatementAST): Unit =
    stat match {
      case ForAST(index, from, to, step) =>
        val cell = assignable(index)

        cell.value = evaln(from)
        cell.loop = Some(LoopControl(line + 1, evaln(to), step.map(evaln).getOrElse(1)))
      case NextAST(index) =>
        access(index) match {
          case None => problem(index.pos, "variable doesn't exist")
          case Some(cell) =>
            if (cell.loop.isEmpty)
              problem(index.pos, "not a for loop index variable")
            else {
              val LoopControl(line, limit, step) = cell.loop.get

              cell.value match {
                case d: Double =>
                  val res = d + step

                  cell.value = res

                  if (step > 0) {
                    if (res <= limit)
                      goto(line)
                  } else {
                    if (res >= limit)
                      goto(line)
                  }
                case v => problem(index.pos, s"loop index variable doesn't contain a number: $v")
              }
            }
        }
      case d @ DimAST(name, dim) =>
        vars get name match {
          case Some(_)          => problem(d.pos, s"array '$name' can't be dimensioned")
          case None if dim <= 0 => problem(d.pos, s"dimension must be positive")
          case None             => vars(name) = Dim(Array.fill(dim)(new Cell(0, None)))
        }
      case NopAST() | RemAST(_) =>
      case PrintAST(args) =>
        args foreach {
          case (expr, Some(";"))      => out.print(show(expr))
          case (expr, None | Some(_)) => out.println(show(expr))
        }
      case LetAST(v: VariableAST, expr) => assignable(v).value = eval(expr)
      case GotoAST(line)                => goto(line)
      case EndAST()                     => loc = null
      case IfAST(cond, thenPart, elsePart) =>
        if (evalb(cond)) perform(line, thenPart) else if (elsePart.isDefined) perform(line, elsePart.get)
    }

  def goto(line: Int): Unit = loc = lines.iteratorFrom(line)

  def index(v: VariableAST): Option[Double] =
    v.sub.map(eval) match {
      case Some(a: Double) if a < 0                  => problem(v.pos, s"an array subscript can't be negative")
      case Some(a: Double) if !a.isWhole             => problem(v.pos, s"an array subscript must be a whole number")
      case Some(_: Double) if !vars.contains(v.name) => problem(v.pos, s"array '${v.name}' has not been dimensioned")
      case Some(_: Double) if !vars(v.name).isInstanceOf[Dim] =>
        problem(v.pos, s"'${v.name}' has not been dimensioned as an array")
      case Some(a: Double) if a >= vars(v.name).asInstanceOf[Dim].array.length =>
        problem(v.pos, s"array subscript out of range: ${a.toLong}")
      case a @ (Some(_: Double) | None) => a.asInstanceOf[Option[Double]]
      case Some(a)                      => problem(v.pos, s"an array subscript must be a number: '$a'")
    }

  def access(v: VariableAST): Option[Cell] =
    vars get v.name match {
      case Some(Dim(array))                 => Some(array(index(v).get.toInt))
      case c @ Some(_: Cell)                => c.asInstanceOf[Option[Cell]]
      case Some(_: BuiltinNumeric | _: Def) => problem(v.pos, s"'${v.name}' is a function")
      case Some(_: Constant)                => problem(v.pos, s"'${v.name}' is a constant")
      case None                             => None
    }

  def assignable(v: VariableAST): Cell =
    access(v) match {
      case None =>
        val cell = new Cell(null, None)

        vars(v.name) = cell
        cell
      case Some(c: Cell) => c
    }

  def show(a: ExpressionAST): String =
    eval(a) match {
      case b: Boolean             => if (b) "TRUE" else "FALSE"
      case w: Double if w.isWhole => w.toLong.toString
      case v                      => String.valueOf(v)
    }

  def eval(expr: ExpressionAST): Any =
    expr match {
      case BooleanAST(b) => b
      case NumberAST(n)  => n
      case StringAST(s)  => s
      case v: VariableAST =>
        access(v) match {
          case None          => problem(v.pos, s"variable '${v.name}' not found")
          case Some(c: Cell) => c.value
        }
      case u @ PrefixAST(op, expr) =>
        (op, eval(expr)) match {
          case ("-", a: Double)    => -a
          case ("NOT", a: Boolean) => !a
          case (o, v)              => problem(u.pos, s"invalid operation: '$o $v'")
        }
      case InfixAST(left, _, "AND", right) => if (!evalb(left)) false else evalb(right)
      case InfixAST(left, _, "OR", right)  => if (evalb(left)) true else evalb(right)
      case InfixAST(left, oppos, op, right) =>
        (eval(left), op, eval(right)) match {
          case (a: Double, "+", b: Double) => a + b
          case (l, "+", r)                 => problem(oppos, s"can't add '$l' and '$r'")
          case (a: Double, "-", b: Double) => a - b
          case (l, "-", r)                 => problem(oppos, s"can't subtract '$r' from '$l'")
          case (a: Double, "*", b: Double) => a * b
          case (l, "*", r)                 => problem(oppos, s"can't multiply '$l' and '$r'")
          case (l: Double, "<", r: Double) => l < r
          case (l: String, "<", r: String) => l < r
          case (l: Double, ">", r: Double) => l > r
          case (l: String, ">", r: String) => l > r
          case (l, "<" | ">", r)           => problem(oppos, s"can't compare '$l' and '$r'")
          case (l, "=", r)                 => l == r
          case _                           => problem(oppos, s"invalid operation")
        }
      case f @ FunctionAST(name, args) =>
        vars get name match {
          case None         => problem(f.pos, s"unknown function '$name'")
          case Some(f: Def) => f(args map eval)
        }
    }

  case class LoopControl(line: Int, limit: Double, step: Double)

  class Cell(var value: Any, var loop: Option[LoopControl])

  case class Constant(value: Any)

  class BuiltinNumeric(name: String, val func: Double => Double) extends Def(name) {
    def apply(args: List[Any]): Double =
      args match {
        case Nil | _ :: _ :: _ => problem(null, s"function '$name' takes one parameter")
        case List(a: Double)   => func(a)
        case List(a)           => problem(null, s"function '$name' expects a number, not '$a'")
      }
  }

  case class Dim(array: Array[Cell])

  abstract class Def(val name: String) extends (List[Any] => Any) {
    def apply(args: List[Any]): Any
  }

}
