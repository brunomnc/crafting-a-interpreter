package parser

import core.Main
import lexer._
import errorhandler.{Failure, ParseError, ParsingExpressionFailure}

case class Parser(tokens: List[Token], var current: Int = 0) {
  def parse: Either[Failure, Expr[String]] =
    try {
      Right(expression)
    } catch {
      case _: Throwable => Left(ParsingExpressionFailure)
    }

  def expression: Expr[String] = equality

  def equality: Expr[String] = {
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

  def comparison: Expr[String] = {
    var expr: Expr[String] = addition
    while (matchT(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous
      val right    = addition
      expr = Binary(expr, operator, right)
    }
    expr
  }

  def addition: Expr[String] = {
    var expr: Expr[String] = multiplication
    while (matchT(MINUS, PLUS)) {
      val operator = previous
      val right    = multiplication
      expr = Binary(expr, operator, right)
    }
    expr
  }

  def multiplication: Expr[String] = {
    var expr = unary
    while (matchT(SLASH, STAR)) {
      val operator = previous
      val right    = unary
      expr = Binary(expr, operator, right)
    }
    expr
  }

  def unary: Expr[String] = {
    if (matchT(BANG, MINUS)) {
      val operator = previous
      val right    = unary
      Unary(operator, right)
    } else {
      primary
    }
  }

  def primary: Expr[String] = {
    if (matchT(FALSE)) return Literal[String]("false")
    if (matchT(TRUE)) return Literal[String]("true")
    if (matchT(NIL)) return Literal[String]("nil")
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
