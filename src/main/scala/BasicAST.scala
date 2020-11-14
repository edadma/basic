package xyz.hyperreal.basic

import scala.util.parsing.input.{Position, Positional}

abstract class BasicAST extends Positional

case class LineAST(line: Int, stat: List[StatementAST], comment: Option[String]) extends BasicAST

case class IntegerAST(v: Int) extends BasicAST

abstract class StatementAST extends BasicAST { val comment: Option[String] = None }
case class DimAST(name: String, dim: IntegerAST) extends StatementAST
case class PrintAST(args: List[(ExpressionAST, Option[String])]) extends StatementAST
case class InputAST(prompt: StringAST, vars: List[String]) extends StatementAST
case class RemAST(rem: String) extends StatementAST
case class LetAST(name: VariableAST, expr: ExpressionAST) extends StatementAST
case class GotoAST(line: IntegerAST) extends StatementAST
case class IfAST(cond: ExpressionAST, thenPart: StatementAST, elsePart: Option[StatementAST]) extends StatementAST
case class ForAST(index: VariableAST, from: ExpressionAST, to: ExpressionAST, step: Option[ExpressionAST])
    extends StatementAST
case class NextAST(index: VariableAST) extends StatementAST
case class NopAST() extends StatementAST
case class EndAST() extends StatementAST

abstract class ExpressionAST extends BasicAST
case class BooleanAST(value: Boolean) extends ExpressionAST
case class NumberAST(value: Double) extends ExpressionAST
case class StringAST(value: String) extends ExpressionAST
case class VariableAST(name: String, sub: Option[ExpressionAST]) extends ExpressionAST
case class FunctionAST(name: String, args: List[ExpressionAST]) extends ExpressionAST
case class InfixAST(left: ExpressionAST, oppos: Position, op: String, right: ExpressionAST) extends ExpressionAST
case class PrefixAST(op: String, expr: ExpressionAST) extends ExpressionAST
