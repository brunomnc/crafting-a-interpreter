package parser

import lexer.Token

trait Expr{
def accept(visitor: Visitor): String
}
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
 def accept(visitor: Visitor): String = visitor.visitBinaryExpr(this)
}
 
case class Grouping(expression: Expr) extends Expr {
 def accept(visitor: Visitor): String = visitor.visitGroupingExpr(this)
}
 
case class Literal(value: Any) extends Expr {
 def accept(visitor: Visitor): String = visitor.visitLiteralExpr(this)
}
 
case class Unary(operator: Token, right: Expr) extends Expr {
 def accept(visitor: Visitor): String = visitor.visitUnaryExpr(this)
}
 
trait Visitor {
  def print(expr: Expr): String
  def visitBinaryExpr(expr: Binary): String
  def visitGroupingExpr(expr: Grouping): String
  def visitLiteralExpr(expr: Literal): String
  def visitUnaryExpr(expr: Unary): String
  def evaluate(expr: Expr): Object
}