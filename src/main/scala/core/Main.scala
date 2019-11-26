package core

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import ErrorHandler._
import lexer.{Scanner, Token, TokenScanner}

import scala.collection.immutable.Stream.continually
import scala.collection.mutable.ListBuffer

object Main extends App {
  args.length match {
    case 0 => runPrompt()
    case 1 => runFile(args(0)).fold(_ => Main, _ => ())
//    case 2 => generateAST
    case _ => System.exit(64)
  }

  def runFile(path: String): Either[Error, Unit] =
    try Files.readAllBytes(Paths.get(path)) match {
      case content: Array[Byte] => Right(run(new String(content, Charset.defaultCharset())))
    } catch {
      case e: Exception => Left(FileDoesNotExist)
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
    val scanner: Scanner          = TokenScanner(source)
    val tokens: ListBuffer[Token] = scanner.scanTokens
    for (token <- tokens) println(token)
  }

}
