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
    case 3 => mockAST
    case _ => error
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
    if (source.equals("exit")) error
    val scanner: Scanner          = TokenScanner(source)
    val tokens: ListBuffer[Token] = scanner.scanTokens
    for (token <- tokens) println(token)
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

  def error(): Unit = {
    println("some error")
    System.exit(0)
  }

  def mockAST(): Unit = {
    val expr =
      Binary(Unary(Token(MINUS, "-", Nil, 1, 1), Literal(123)), Token(STAR, "*", Nil, 1, 1), Grouping(Literal(321)))
    println(new AstPrinter().print(expr))
  }
}
