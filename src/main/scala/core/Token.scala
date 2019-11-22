package core

case class Token(
    tokenType: TokenType,
    lexeme: String,
    literal: Object,
    line: Int
) {

  override def toString: String = tokenType + " " + lexeme + " " + literal + " " + line
}
