package parser

import java.io.PrintWriter
import errorhandler._

case class GenerateAST(basename: String, types: List[String]) {
  implicit val w = new PrintWriter("src/main/scala/parser/Expr.scala", "UTF-8")

  def defineAST: Either[Failure, Success] = for {
    header <- defineHeader
    defType <- defineType(basename)
    result <- defineVisitor(basename, types)
  } yield result

  def defineHeader: Either[ASTTypeFailure, Success] = {
    try {
      w.println("package parser")
      w.println()
      w.println("import lexer.Token")
      w.println()
      w.println(s"trait $basename{")
      w.println(  "def accept(visitor: Visitor): String")
      w.println("}")
      Right(ASTSuccess)
    } catch {
      case _: Throwable => Left(ASTTypeFailure)
    }
  }

  def defineType(baseName: String): Either[ASTTypeFailure, Success] = {
    try {
      for (t <- types) {
        val className: String = t.split(">")(0).trim
        val fields: String    = t.split(">")(1).trim
        w.println(s"case class $className(${fields}) extends $baseName {")
        w.println(s" def accept(visitor: Visitor): String = visitor.visit$className$baseName(this)")
        w.println("}")
        w.println(" ")
      }
      Right(ASTSuccess)
    } catch {
      case _: Throwable => Left(ASTTypeFailure)
    }
  }

  def defineVisitor(baseName: String, types: List[String]): Either[ASTVisitorFailure, Success] = {
    try {
      w.println("trait Visitor {")
      w.println(s"  def print(${baseName.toLowerCase}: $baseName): String")
      for (tType <- types) {
        val typeName: String = tType.split(">")(0).trim
        w.println(s"  def visit$typeName$basename(${baseName.toLowerCase}: $typeName): String")
      }
      w.println("}")
      w.close()
      Right(ASTSuccess)
    } catch {
      case _: Throwable => Left(ASTVisitorFailure)
    }
  }
}
