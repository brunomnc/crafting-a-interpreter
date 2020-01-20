package parser

class AstPrinter extends Visitor[String] {
  def print(expr: Expr[String]): String = expr.accept(this)

  def visitBinaryExpr(expr: Binary[String]): String =
    parenthesize(expr.operator.lexeme, expr.left, expr.right)

  def visitGroupingExpr(expr: Grouping[String]): String =
    parenthesize("group", expr.expression)

  def visitLiteralExpr(expr: Literal[String]): String = if (expr.value == null) " " else expr.value.toString

  def visitUnaryExpr(expr: Unary[String]): String = parenthesize(expr.operator.lexeme, expr.right)

  def parenthesize(name: String, expr: Expr[String]*): String = {
    val builder: StringBuilder = new StringBuilder
    builder.append("(").append(name)
    for (exprs <- expr) {
      builder.append(" ")
      builder.append(exprs.accept(this))
    }
    builder.append(")")
    builder.toString
  }

  def evaluate(expr: Expr[String]): String = expr.accept(this)
}
