package parser

import errorhandler.{Failure, ParseError, RuntimeError}
import lexer._

class Interpreter extends Visitor {
  def print(expr: Expr): String = ???

  def visitBinaryExpr(expr: Binary): Object = {
    val left: Object = evaluate(expr.left)
    val right: Object = evaluate(expr.right)

    case expr.operator.tokenType match {
      case GREATER => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] > right.asInstanceOf[Double]
      }
      case GREATER_EQUAL => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] >= right.asInstanceOf[Double]
      }
      case LESS => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] < right.asInstanceOf[Double]
      }
      case LESS_EQUAL => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] <= right.asInstanceOf[Double]
      }
      case MINUS => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] - right.asInstanceOf[Double]
      }
      case SLASH => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] / right.asInstanceOf[Double]
      }
      case STAR => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] * right.asInstanceOf[Double]
      }
      case BANG => !isEqual(left, right)
      case BANG_EQUAL => isEqual(left, right)
      case PLUS => if(left.isInstanceOf[Double] && right.isInstanceOf[Double])
        left.asInstanceOf[Double] + right.asInstanceOf[Double]
      else if(left.isInstanceOf[String] && right.isInstanceOf[String])
        left.asInstanceOf[String] + right.asInstanceOf[String] else throw RuntimeError(s"Operands must be two numbers or two strings: ${expr.operator}")
      case _ => None
    }
  }

  def visitGroupingExpr(expr: Grouping): Object = evaluate(expr.expression)

  def visitLiteralExpr(expr: Literal): Object = expr.value

  def visitUnaryExpr(expr: Unary): Object = {
    val right: Object = evaluate(expr.right)

    expr.operator.tokenType match {
      case MINUS => {
        -right.asInstanceOf[Double]
        checkNumberOperand(expr.operator, right)
      }
      case BANG  => !isTruthy(right)
      case _     => None
    }
  }

  def checkNumberOperand(operator:Token, operand: Object): Either[Failure, Object] = {
    if(operand.isInstanceOf[Double]) return Right(operand)
    Left(RuntimeError(s"Operand must be a number: $operator"))
  }

  def checkNumberOperands(operator: Token, left: Object, right: Object): Either[Failure, Unit] = {
    if(left.isInstanceOf[Double] && right.isInstanceOf[Double]) Right(())
    Left(RuntimeError(s"Operands must be numbers: $operator"))
  }

  def isTruthy(value: Object): Boolean = {
    if(value == None) false
    else if(value.isInstanceOf[Boolean]) value.asInstanceOf[Boolean]
    else false
  }

  def isEqual(a: Object, b: Object): Boolean = {
    if(a == None && b == None) true
    else if(a == None) false
    else a.equals(b)
  }
  def evaluate(expr: Expr): Object = expr.accept(this)
}
