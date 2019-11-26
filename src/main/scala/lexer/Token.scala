package lexer

case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: Object,
    line: Int,
    column: Int
) {

  override def toString: String = tokenType + " " + lexeme + " " + literal + " " + line + ", " + column
}
