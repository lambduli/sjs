package lexer

import scala.util.parsing.combinator._

import lexer.token._
import lexer.error._

object LipoLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f\n]+".r

  def comment: Parser[COMMENT] = {
    ";;.*".r ^^ { comment => COMMENT(comment) }
  }

  def identifier: Parser[IDENTIFIER] = {
    "[a-zA-Z_][[.a-zA-Z0-9_]+[a-zA-Z0-9_]]*".r ^^ { str => IDENTIFIER(str) }
  }

  def string: Parser[STRING] = {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
        STRING(content)
      }
  }

  def ifclause = "if" ^^ (_ => IF)

  def quote = "['`]".r ^^ (_ => QUOTE)

  def comma = "," ^^ (_ => COMMA)

  def left = "(" ^^ (_ => LEFT)
  def right = ")" ^^ (_ => RIGHT)

  def define = "define" ^^ (_ => DEFINE)
  def lambda = "lambda" ^^ (_ => LAMBDA)

  def `import` = "import" ^^ (_ => IMPORT)
  def export = "export" ^^ (_ => EXPORT)

  def number = """\d+(\.\d*)?""".r ^^ { str => NUMBER(str.toDouble) }

  def false_ = "false" ^^ { f => BOOL(false)}
  def true_ = "true" ^^ { t => BOOL(true) }

  def nil = "nil" ^^ (_ => NIL)
  def is_null = "null" ^^ { case str => IDENTIFIER("is_null") }
  
  def plus = "+" ^^ (_ => PLUS)
  def minus = "-" ^^ (_ => MINUS)
  def multiply = "*" ^^ (_ => MULTIPLY)
  def divide = "/" ^^ (_ => DIVIDE)
  def equal = "=" ^^ (_ => EQUAL)
  def lesser = "<" ^^ (_ => LESSER)
  def greater = ">" ^^ (_ => GREATER)
  def lessereq = "<=" ^^ (_ => LESSEREQ)
  def greatereq = ">=" ^^ (_ => GREATEREQ)
  def and = "and" ^^ (_ => AND)
  def or = "or" ^^ (_ => OR)

  def tokens: Parser[List[Token]] = {
    phrase(rep1(
      comment
      | true_ | false_ | define | ifclause | lambda | nil | is_null | `import` | export
      | number | plus | minus | multiply | divide | equal | lesser | greater | lessereq | greatereq | and | or
      | identifier | string | number | quote | comma | left | right))
  }

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}