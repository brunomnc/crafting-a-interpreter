package lexer

case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: Any,
    line: Int,
    column: Int
) {

  override def toString: String = tokenType + " " + lexeme + " " + literal + " " + line + ", " + column
}
