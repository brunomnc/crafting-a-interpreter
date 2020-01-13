package parser

import core.Main
import lexer._
import errorhandler.ParseError

case class Parser(tokens: List[Token], var current: Int = 0) {
  def parse: Either[Unit, Expr] =
    try {
      Right(expression)
    } catch {
      case _: Throwable => Left(())
    }

  def expression: Expr = equality

  def equality: Expr = {
    val expr = comparison
    while (matchT(BANG_EQUAL, EQUAL_EQUAL)) {
      Binary(expr, previous, comparison)
    }
    expr
  }

  def matchT(types: TokenType*): Boolean = {
    for (t <- types) {
      if (check(t)) {
        advance
        return true
      }
    }
    false
  }

  def check(tokenType: TokenType): Boolean =
    if (isAtEnd) false else peek.tokenType == tokenType

  def advance: Token = {
    if (!isAtEnd)
      current += 1
    previous
  }

  def isAtEnd: Boolean = peek.tokenType == EOF

  def peek: Token = tokens(current)

  def previous: Token = tokens(current - 1)

  def comparison: Expr = {
    var expr: Expr = addition
    while (matchT(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous
      val right    = addition
      expr = Binary(expr, operator, right)
    }
    expr
  }

  def addition: Expr = {
    var expr: Expr = multiplication
    while (matchT(MINUS, PLUS)) {
      val operator = previous
      val right    = multiplication
      expr = Binary(expr, operator, right)
    }
    expr
  }

  def multiplication: Expr = {
    var expr = unary
    while (matchT(SLASH, STAR)) {
      val operator = previous
      val right    = unary
      expr = Binary(expr, operator, right)
    }
    expr
  }

  def unary: Expr = {
    if (matchT(BANG, MINUS)) {
      val operator = previous
      val right    = unary
      Unary(operator, right)
    } else {
      primary
    }
  }

  def primary: Expr = {
    if (matchT(FALSE)) return Literal(false)
    if (matchT(TRUE)) return Literal(true)
    if (matchT(NIL)) return Literal(Nil)
    if (matchT(NUMBER, STRING)) return Literal(previous.literal)
    if (matchT(LEFT_PAREN)) {
      val expr = expression
      consume(RIGHT_PAREN, "Expected ) after expression")
      return Grouping(expr)
    }
    throw new Exception
  }

  def consume(tokenType: TokenType, message: String): Either[ParseError, Token] =
    if (check(tokenType)) Right(advance) else Left(Main.error(peek, message))

  def synchronize(): Unit = {
    advance
    while (!isAtEnd) {
      if (previous.tokenType == SEMICOLON) return
      peek.tokenType match {
        case CLASS  => return
        case FUN    => return
        case VAR    => return
        case FOR    => return
        case IF     => return
        case WHILE  => return
        case PRINT  => return
        case RETURN => return
      }
      advance
    }
  }
}
