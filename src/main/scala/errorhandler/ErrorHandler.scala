package errorhandler

sealed trait Failure {
  def err: String
  println(err)
}
sealed trait Success

trait IOFailure extends Failure
trait LexicalFailure extends Failure
trait ParsingFailure extends Failure

case object ASTSuccess extends Success

case object UnrecognizedToken extends LexicalFailure {
  override def err: String = "Could not recognize a token"
}

object FileFailure extends IOFailure {
  override def err: String = "File does not exist"
}

object ASTFailure extends ParsingFailure {
  override def err: String = "Could not generate AST"
}

object ASTTypeFailure extends ParsingFailure {
  override def err: String = "Error while building AST Types"
}

object ASTVisitorFailure extends ParsingFailure {
  override def err: String = "Error while building Visitors"
}

object ParsingExpressionFailure extends ParsingFailure {
  def err: String = "Error while building an expression"
}

case class ParseError(error: String) extends RuntimeException

case class RuntimeError(error: String) extends  RuntimeException with Failure {
  def err: String = error
}