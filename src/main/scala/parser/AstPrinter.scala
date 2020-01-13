package parser

class AstPrinter extends Visitor {
  def print(expr: Expr): String = expr.accept(this)

  def visitBinaryExpr(expr: Binary): String =
    parenthesize(expr.operator.lexeme, expr.left, expr.right)

  def visitGroupingExpr(expr: Grouping): String =
    parenthesize("group", expr.expression)

  def visitLiteralExpr(expr: Literal): String = if(expr.value == null) " " else expr.value.toString

  def visitUnaryExpr(expr: Unary): String = parenthesize(expr.operator.lexeme, expr.right)

  def parenthesize(name: String, expr: Expr*): String = {
    val builder: StringBuilder = new StringBuilder
    builder.append("(").append(name)
    for (exprs <- expr) {
      builder.append(" ")
      builder.append(exprs.accept(this))
    }
    builder.append(")")
    builder.toString
  }

  def evaluate(expr: Expr): AnyRef = ???
}
