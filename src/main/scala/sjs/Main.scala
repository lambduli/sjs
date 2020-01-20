package main

import java.io.PrintWriter

import lexer._
import lexer.error._

import parser._
import parser.ast._

import template._

object Compiler {
  def apply(code: String): Either[CompilationError, AST] = {
    for {
      tokens <- LipoLexer(code).right
      ast <- LipoParser(tokens).right
    } yield ast
  }
}

object Main extends App {

  println(s"Compiling file: ${args(0)}")

  val source = io.Source.fromInputStream(getClass.getResourceAsStream(s"/${args(0)}"))
  val text = try source.mkString finally source.close()

  Compiler(text).right.map(code => {
    new PrintWriter("a.js") { write(code + template.builtins); close }
  })

  // Compiler(text).left.map(error => {
  //   println("\n")
  //   println(error)
  //   println("\n")
  // })
}