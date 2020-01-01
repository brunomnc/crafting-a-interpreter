package errorhandler

sealed trait Failure {
  def err: String
  println(err)
}

sealed trait Success

case object ASTSuccess extends Success

case object FileFailure extends Failure {
  override def err: String = "File does not exist"
}

case object ASTFailure extends Failure {
  override def err: String = "Could not generate AST"
}

trait ASTTypeFailure extends Failure
trait ASTVisitorFailure extends Failure

object ASTTypeFailure extends ASTTypeFailure {
  override def err: String = "Error while building AST Types"
}

object ASTVisitorFailure extends ASTVisitorFailure {
  override def err: String = "Error while building Visitors"
}
