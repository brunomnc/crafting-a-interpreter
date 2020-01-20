package parser

import errorhandler.{Failure, ParseError, RuntimeError}
import lexer._

class Interpreter extends Visitor[Any] {
  def interpret(expr: Expr[Any]): Unit = {
    val value = evaluate(expr)
    println(stringify(value))
  }

  def stringify(any: Any): String = {
    any match {
      case Nil => return "nil"
      case o : Double =>
        var text = o.toString
        if(text.endsWith(".0")) {
          text = text.substring(0, text.length - 2)
        } else {
          return text
        }
    }
    any.toString
  }

  def print(expr: Expr[Any]): Any = ???

  def visitBinaryExpr(expr: Binary[Any]): Any = {
    val left: Any  = evaluate(expr.left)
    val right: Any = evaluate(expr.right)

    expr.operator.tokenType match {
      case GREATER =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] > right.asInstanceOf[Double]
      case GREATER_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] >= right.asInstanceOf[Double]
      case LESS =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] < right.asInstanceOf[Double]
      case LESS_EQUAL =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] <= right.asInstanceOf[Double]
      case MINUS =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] - right.asInstanceOf[Double]
      case SLASH =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] / right.asInstanceOf[Double]
      case STAR =>
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] * right.asInstanceOf[Double]
      case BANG       => !isEqual(left, right)
      case BANG_EQUAL => isEqual(left, right)
      case PLUS =>
        left match {
          case d: Double   => if (right.isInstanceOf[Double]) d + right.asInstanceOf[Double]
          case str: String => if (right.isInstanceOf[String]) str + right.asInstanceOf[String]
          case _           => throw RuntimeError(s"Operands must be two numbers or two strings: ${expr.operator}")
        }
      case _ => ()
    }
  }

  def visitGroupingExpr(expr: Grouping[Any]): Any = evaluate(expr.expression)

  def visitLiteralExpr(expr: Literal[Any]): Any = expr.value

  def visitUnaryExpr(expr: Unary[Any]): Any = {
    val right: Any = evaluate(expr.right)

    expr.operator.tokenType match {
      case MINUS =>
        -right.asInstanceOf[Double]
        checkNumberOperand(expr.operator, right).fold(_ => (), c => c)
      case BANG => !isTruthy(right)
      case _    => ()
    }
  }

  def checkNumberOperand(operator: Token, operand: Any): Either[Failure, Any] = {
    if (operand.isInstanceOf[Double]) return Right(operand)
    Left(RuntimeError(s"Operand must be a number: $operator"))
  }

  def checkNumberOperands(operator: Token, left: Any, right: Any): Either[Failure, Unit] = {
    if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) Right(())
    Left(RuntimeError(s"Operands must be numbers: $operator"))
  }

  def isTruthy(value: Any): Boolean = {
    if (value == None) false
    else if (value.isInstanceOf[Boolean]) value.asInstanceOf[Boolean]
    else false
  }

  def isEqual(a: Any, b: Any): Boolean = {
    if (a == None && b == None) true
    else if (a == None) false
    else a.equals(b)
  }

  def evaluate(expr: Expr[Any]): Any = expr.accept(this)
}
