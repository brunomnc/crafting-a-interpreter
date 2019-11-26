package parser

import java.io.PrintWriter

class GenerateAST(outputDir: String, basename: String, types: List[String]) {
  val path: String = outputDir + "/" + basename + ".java"
  val writer       = new PrintWriter(path, "UTF-8")

  def defineAST: Unit = {
    writer.println("package parser")
    writer.println()
    writer.println("import java.util.List")
    writer.println()
    writer.println("abstract class " + basename + " {")
    for (t <- types) {
      val className: String = t.split(":")(0).trim
      val fields: String    = t.split(":")(1).trim
      defineType(writer, basename, className, fields)
    }
    writer.println("}")
    writer.close()
  }

  def defineType(writer: PrintWriter, basename: String, className: String, fieldList: String) = ???
  //Todo: https://www.craftinginterpreters.com/representing-code.html < ctrl+f definetype

}
