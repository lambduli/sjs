package parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}

import lexer._
import lexer.token._
import lexer.error._

import parser.ast._

object LipoParser extends Parsers {
  override type Elem = Token

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }

  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new TokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  private def comment: Parser[AST] = {
    accept("comment", { case comment @ COMMENT(content) => Comment(content) })
  }

  private def identifier: Parser[AST] = {
    accept("identifier", { case id @ IDENTIFIER(name) => Identifier(name) })
  }

  private def operator: Parser[AST] = {
    (PLUS | MINUS | MULTIPLY | DIVIDE | EQUAL | LESSER | GREATER | LESSEREQ | GREATEREQ | AND | OR) ^^ { case op => Operator(op)}
  }

  private def stringlit: Parser[AST] = {
    accept("string literal", { case str @ STRING(value) => StringLit(value) })
  }

  private def numberlit: Parser[AST] = {
    accept("numeric literal", { case num @ NUMBER(value) => NumberLit(value) })
  }

  private def boollit: Parser[AST] = {
    accept("boolean literal", { case b @ BOOL(value) => BoolLit(value) })
  }

  private def nillit: Parser[AST] = {
    accept("nil literal", { case nil @ NIL => Nil })
  }

  private def value: Parser[AST] = {
    (stringlit | numberlit | boollit | lambdalit | nillit) ^^ { case v => v }
  }

  def program: Parser[AST] = {
    rep1(lispram | comment | `import` | export | define) ^^ { case exps => Program(exps) }
  }

  def lispram: Parser[AST] = {
    codelist | operator | identifier | ifexp  | value | quotedlist |  letexp
  }

  def quotedlispram: Parser[AST] = {
    unquotedcodelist | operator | identifier | ifexp | define | value | implicitlyquotedlist
  }

  def implicitlyquotedlist: Parser[AST] = {
    (LEFT ~ rep(quotedlispram) ~ RIGHT) ^^ { case left ~ valList ~ right => ValList(valList)}
  }

  def quotedlist: Parser[AST] = {
    (QUOTE ~ LEFT ~ rep(quotedlispram) ~ RIGHT) ^^ { case quote ~ left ~ valList ~ right => ValList(valList)}
  }
  
  def unquotedcodelist: Parser[AST] = {
    (COMMA ~ LEFT ~ rep1(lispram) ~ RIGHT) ^^ { case comma ~ left ~ expList ~ right => AppList(expList) }
  }
  
  def codelist: Parser[AST] = {
    (LEFT ~ rep1(lispram) ~ RIGHT) ^^ { case left ~ expList ~ right => AppList(expList) }
  }

  def arglist: Parser[AST] = {
    (LEFT ~ rep1(lispram) ~ RIGHT) ^^ { case left ~ argList ~ right => ArgList(argList)}
  }

  def ifexp: Parser[AST] = {
    (LEFT ~ IF ~ (boollit | codelist) ~ (value | codelist) ~ (value | codelist) ~ RIGHT) ^^ {
      case left ~ if_ ~ condition ~ then_ ~ else_ ~ right => IfThenElse(condition, then_, else_)
    }

  }

  def define: Parser[AST] = {
    (LEFT ~ DEFINE ~ identifier ~ arglist ~ lispram ~ RIGHT) ^^ {
      case left ~ define ~ Identifier(name) ~ args ~ body ~ right => FnDef(name, args, body)
    }
  }

  def lambdalit: Parser[AST] = {
    (LEFT ~ LAMBDA ~ arglist ~ lispram ~ RIGHT) ^^ {
      case left ~ lambda ~ args ~ body ~ right => Lambda(args, body)
    }
  }

  def `import`: Parser[AST] = {
    (LEFT ~ IMPORT ~ QUOTE ~ LEFT ~ rep1(identifier) ~ RIGHT ~ stringlit ~ RIGHT) ^^ {
      case left ~ imp ~ q ~ l ~ ids ~ r ~ StringLit(path) ~ right => Import(ids, path)
    }
  }

  def export: Parser[AST] = {
    (LEFT ~ EXPORT ~ identifier ~ value.? ~ RIGHT) ^^ {
      case left ~ exp ~ Identifier(id) ~ statement ~ right => Export(id, statement)
    }
  }

  def letexp: Parser[AST] = {
    (LEFT ~ LET ~ LEFT ~ rep1(vardef) ~ RIGHT ~ lispram ~ RIGHT) ^^ {
      case left ~ let ~ l ~ varlist ~ r ~ body ~ right => LetExp(varlist, body)
    }
  }

  def vardef: Parser[(String, AST)] = {
    (LEFT ~ identifier ~ lispram ~ RIGHT) ^^ {
      case left ~ Identifier(id) ~ value ~ right => (id, value)
    }
  }
}