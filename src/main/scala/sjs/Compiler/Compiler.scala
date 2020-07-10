package compiler

import parser.ast._

trait Compiler {
  def compile(root: AST): String
}