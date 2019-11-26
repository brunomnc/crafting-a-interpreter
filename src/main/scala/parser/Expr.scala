package parser

import lexer.Token

sealed trait Expr

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
