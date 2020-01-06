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

case object FileFailure extends Failure {
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

case class ParseError(error: String) extends RuntimeException