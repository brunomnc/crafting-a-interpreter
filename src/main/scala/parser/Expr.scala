package parser

import lexer.Token

trait Expr[T <: Any] {
def accept(visitor: Visitor[T]): T
}

case class Binary[T](left: Expr[T], operator: Token, right: Expr[T]) extends Expr[T] {
 def accept(visitor: Visitor[T]): T = visitor.visitBinaryExpr(this)
}
 
case class Grouping[T](expression: Expr[T]) extends Expr[T] {
 def accept(visitor: Visitor[T]): T = visitor.visitGroupingExpr(this)
}
 
case class Literal[T](value: Any) extends Expr[T] {
 def accept(visitor: Visitor[T]): T = visitor.visitLiteralExpr(this)
}
 
case class Unary[T](operator: Token, right: Expr[T]) extends Expr[T] {
 def accept(visitor: Visitor[T]): T = visitor.visitUnaryExpr(this)
}
 

trait Visitor[T] {
  def print(expr: Expr[T]): T
  def visitBinaryExpr(expr: Binary[T]): T
  def visitGroupingExpr(expr: Grouping[T]): T
  def visitLiteralExpr(expr: Literal[T]): T
  def visitUnaryExpr(expr: Unary[T]): T
  def evaluate(expr: Expr[T]): T
}