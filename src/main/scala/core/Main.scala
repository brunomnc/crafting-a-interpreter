package core

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import scala.collection.immutable.Stream.continually
import scala.collection.mutable.ListBuffer

object Main extends App {
  args.length match {
    case 0 => runPrompt()
    case 1 => runFile(args(0))
    case _ => System.exit(64)
  }

  def runFile(path: String): Unit = {
    val bytes: Array[Byte] = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset()))
  }

  def runPrompt(): Unit = {
    println("Welcome to this bs")
    val input: InputStreamReader = new InputStreamReader(System.in)
    val reader: BufferedReader = new BufferedReader(input)
    for(a <- continually()) {
      println(">>>")
      run(reader.readLine())
    }
  }

  def run(source: String): Unit = {
    val scanner:Scanner = TokenScanner(source)
    val tokens: ListBuffer[Token] = scanner.scanTokens
    for(token <- tokens) println(token)
  }

}
