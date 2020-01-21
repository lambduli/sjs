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
  val input = args(0)

  println(s"Compiling file: $input")

  val source = io.Source.fromInputStream(getClass.getResourceAsStream(s"/$input"))
  val text = try source.mkString finally source.close()

  Compiler(text).right.map(code => {
    val ext = ".sjs".r
    val translated = ext.replaceAllIn(input, ".mjs")
    new PrintWriter(translated) { write(code + template.builtins); close }
  })

  Compiler(text).left.map(error => {
    println("\n")
    println(error)
    println("\n")
  })
}