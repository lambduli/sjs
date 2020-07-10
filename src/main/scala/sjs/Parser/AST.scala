package parser.ast

import lexer.token._

sealed trait AST

case class Program(content: List[AST]) extends AST

case class Identifier(value: String) extends AST
case class Operator(value: Token) extends AST

case class NumberLit(value: Number) extends AST
case class StringLit(value: String) extends AST
case class BoolLit(value: Boolean) extends AST

case class LetExp(varlist: List[(String, AST)], body: AST) extends AST

case class AppList(value: List[AST]) extends AST
case class ValList(value: List[AST]) extends AST
case class ArgList(value: List[AST]) extends AST

case class Import(ids: List[AST], path: String) extends AST

case class Export(id: String, expression: Option[AST]) extends AST

case class FnDef(name: String, args: AST, body: AST) extends AST
case class Lambda(args: AST, body: AST) extends AST

case object Nil extends AST

case class IfThenElse(condition: AST, `then`: AST, `else`: AST) extends AST

case class Comment(content: String) extends AST