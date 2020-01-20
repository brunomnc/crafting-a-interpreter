package core

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import errorhandler._
import lexer._
import parser._

import scala.collection.immutable.Stream.continually
import scala.collection.mutable.ListBuffer

object Main extends App {
  args(0).toInt match {
    case 0 => runPrompt()
    case 1 => runFile(args(1)).fold(_ => Main, _ => ())
    case 2 => buildAST
    case 3 => mockAST()
    case _ => halt()
  }

  def runFile(path: String): Either[Failure, Unit] =
    try Files.readAllBytes(Paths.get(path)) match {
      case content: Array[Byte] => Right(run(new String(content, Charset.defaultCharset())))
    } catch {
      case e: Exception => Left(FileFailure)
    }

  def runPrompt(): Unit = {
    println("Bitch ass interpreter ")
    val input: InputStreamReader = new InputStreamReader(System.in)
    val reader: BufferedReader   = new BufferedReader(input)
    for (a <- continually()) {
      println(">>")
      run(reader.readLine())
    }
  }

  def run(source: String): Unit = {
    if (source.equals("exit")) halt()
    val scanner: Scanner          = TokenScanner(source)
    val tokens: ListBuffer[Token] = scanner.scanTokens
    for (token <- tokens) println(token)
    val parser: Parser            = Parser(tokens.toList)
    parser.parse.fold(_ => halt(), e => new AstPrinter().print(e))
//    val interpreter = new Interpreter
//    interpreter.interpret()
  }

  def buildAST: Either[Failure, Success] = {
    for {
      ast <- GenerateAST(
        "Expr",
        List(
          "Binary   > left: Expr, operator: Token, right: Expr",
          "Grouping > expression: Expr",
          "Literal  > value: Any",
          "Unary    > operator: Token, right: Expr"
        )
      ).defineAST
    } yield ast
  }

  def halt(): Unit = {
    System.exit(0)
  }

  def mockAST(): Unit = {
    val expr =
      Binary[String](Unary[String](Token(MINUS, "-", Nil, 1, 1), Literal[String](123)), Token(STAR, "*", Nil, 1, 1), Grouping[String](Literal(321)))
    println(new AstPrinter().print(expr))
  }

  def report(line: Int, where: String, message: String): String = {
    val errorString = s"[line, $line], Error $where: $message"
    println(errorString)
    state.error = true
    errorString
  }

  def error(token: Token, message: String): ParseError = {
    state.error = true
    if (token.tokenType == EOF) ParseError(report(token.line, " at end", message))
    else ParseError(report(token.line, s" at $token.lexeme '", message))
  }
}

object state {
  var error = false
}
