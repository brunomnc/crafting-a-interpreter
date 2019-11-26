package core

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Try

trait Scanner {
  def scanTokens: ListBuffer[Token]
  def isAtEnd: Boolean
  def scanToken: Unit
  def advance: Char
  def addToken(tokenType: TokenType): Unit
  def addToken(tokenType: TokenType, literal: Option[Any]): Unit
  def next(expected: Char): Boolean
  def peek: Char
  def isDigit(c: Char): Boolean
  def number(): Unit
  def peekNext: Char
  def isAlpha(c: Char): Boolean
  def isAlphaNumeric(c: Char): Boolean
  def identifier(): Unit
}

case class TokenScanner(source: String) extends Scanner {
  var tokens: ListBuffer[Token] = ListBuffer[Token]()
  var start: Int                = 0
  var current: Int              = 0
  var line: Int                 = 0

  val keywords: HashMap[String, TokenType] = HashMap(
    "and"    -> AND,
    "class"  -> CLASS,
    "else"   -> ELSE,
    "false"  -> FALSE,
    "for"    -> FOR,
    "fun"    -> FUN,
    "if"     -> IF,
    "nil"    -> NIL,
    "or"     -> OR,
    "print"  -> PRINT,
    "return" -> RETURN,
    "super"  -> SUPER,
    "this"   -> THIS,
    "true"   -> TRUE,
    "var"    -> VAR,
    "while"  -> WHILE,
  )

  def scanTokens: ListBuffer[Token] = {
    while (!isAtEnd) {
      start = current
      scanToken
    }
    tokens += Token(EOF, "", null, line)
    tokens
  }

  def isAtEnd: Boolean = current >= source.length

  def scanToken: Unit = {
    advance match {
      case '('  => addToken(LEFT_PAREN)
      case ')'  => addToken(RIGHT_PAREN)
      case '{'  => addToken(LEFT_BRACE)
      case '}'  => addToken(RIGHT_BRACE)
      case '.'  => addToken(DOT)
      case ','  => addToken(COMMA)
      case '-'  => addToken(MINUS)
      case '+'  => addToken(PLUS)
      case ';'  => addToken(SEMICOLON)
      case '*'  => addToken(STAR)
      case '('  => addToken(LEFT_PAREN)
      case ')'  => addToken(RIGHT_PAREN)
      case '!'  => addToken(if (next('=')) EQUAL_EQUAL else EQUAL)
      case '/'  => if (next('/')) { while (peek != '/' && !isAtEnd) advance } else addToken(SLASH)
      case '\n' => line += 1
      case '\r' => line += 1
      case '\t' => line += 1
      case c    => if (isDigit(c)) number else if (isAlpha(c)) identifier
    }
  }

  def advance: Char = {
    current += 1
    source(current - 1)
  }

  def addToken(tokenType: TokenType): Unit = {
    addToken(tokenType, None)
  }

  def addToken(tokenType: TokenType, literal: Option[Any]): Unit = {
    val text: String = source.substring(start, current)
    tokens += Token(tokenType, text, literal, line)
  }

  def next(expected: Char): Boolean = {
    if (isAtEnd) return false
    if (source(current) != expected) return false
    current += 1
    true
  }

  def peek: Char = if (isAtEnd) '\u0000' else source(current)

  def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  def number: Unit = {
    while (isDigit(peek)) advance
    if (peek == '.' && isDigit(peekNext)) {
      advance
      while (isDigit(peek)) advance
    }
    addToken(NUMBER, Try(source.substring(start, current).toDouble).toOption)
  }

  def peekNext: Char = if (current + 1 >= source.length) '0' else source.charAt(current + 1)

  def isAlpha(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

  def identifier: Unit = {
    while (isAlphaNumeric(peek)) advance
    val text: String         = source.substring(start, current)
    val tokenType: TokenType = keywords.getOrElse(text, IDENTIFIER)
    addToken(tokenType)
  }
}
