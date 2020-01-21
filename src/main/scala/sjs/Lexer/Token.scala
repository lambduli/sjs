package lexer.token

sealed trait Token

case class IDENTIFIER(str : String) extends Token

case object IF extends Token

case object QUOTE extends Token
case object COMMA extends Token

case object LEFT extends Token
case object RIGHT extends Token

case object DEFINE extends Token
case object LAMBDA extends Token

case object IMPORT extends Token
case object EXPORT extends Token

case class NUMBER(double : Double) extends Token
case class STRING(str : String) extends Token
case class BOOL(bool : Boolean) extends Token
case object NIL extends Token

case object PLUS extends Token {
  override def toString = "+"
}
case object MINUS extends Token {
  override def toString = "-"
}
case object MULTIPLY extends Token {
  override def toString = "*"
}
case object DIVIDE extends Token {
  override def toString = "/"
}
case object EQUAL extends Token {
  override def toString = "==="
}
case object LESSER extends Token {
  override def toString = "<"
}
case object GREATER extends Token {
  override def toString = ">"
}
case object LESSEREQ extends Token {
  override def toString = "<="
}
case object GREATEREQ extends Token {
  override def toString = ">="
}

case object AND extends Token {
  override def toString = "&&"
}
case object OR extends Token {
  override def toString = "||"
}