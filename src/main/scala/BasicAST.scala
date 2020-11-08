package xyz.hyperreal.basic

import scala.util.parsing.input.{Position, Positional}

abstract class BasicAST extends Positional

case class LineAST(line: Int, stat: StatementAST, comment: Option[String]) extends BasicAST

abstract class StatementAST extends BasicAST { val comment: Option[String] = None }
case class PrintAST(args: List[(ExpressionAST, Option[String])]) extends StatementAST
case class InputAST(prompt: StringAST, vars: List[String]) extends StatementAST
case class RemAST(rem: String) extends StatementAST
case class LetAST(name: VariableAST, expr: ExpressionAST) extends StatementAST
case class GotoAST(line: Int) extends StatementAST
case class IfAST(cond: ExpressionAST, thenPart: StatementAST, elsePart: Option[StatementAST]) extends StatementAST
case class ForAST(name: VariableAST, from: Int, to: Int, step: Option[Int]) extends StatementAST
case class NextAST(name: Option[VariableAST]) extends StatementAST
case class NopAST() extends StatementAST
case class EndAST() extends StatementAST

abstract class ExpressionAST extends BasicAST
case class NumberAST(value: Double) extends ExpressionAST
case class StringAST(value: String) extends ExpressionAST
case class VariableAST(name: String) extends ExpressionAST
case class FunctionAST(name: String, args: List[ExpressionAST]) extends ExpressionAST
case class InfixAST(left: ExpressionAST, oppos: Position, op: String, right: ExpressionAST) extends ExpressionAST
case class PrefixAST(op: String, expr: ExpressionAST) extends ExpressionAST
