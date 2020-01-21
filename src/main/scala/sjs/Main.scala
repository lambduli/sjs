package main

import java.io.PrintWriter

import lexer._
import lexer.error._

import parser._
import parser.ast._

import template._

object Compiler {
  var s : Set[String] = _

  def apply(input: String, $s : Set[String]): Either[CompilationError, AST] = {
    println(s"\n\nCompiling file: $input\n\n")

    s = $s
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(s"/$input"))
    val text = try source.mkString finally source.close()
    val ast = compile(text)

    ast.right.map(program => {
      program match {
        case Program(statements) => for (statement <- statements) {
          statement match {
            case Import(ids, path) => if (path.contains(".sjs") && ! alreadyDone(path)) {
              val prefix = "[.]/".r
              val trimmed = prefix.replaceAllIn(path, "")
              Compiler(trimmed, s)
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    })
    
    ast.right.map(code => {
      val ext = ".sjs".r
      val translated = ext.replaceAllIn(input, ".mjs")
      new PrintWriter(translated) { write(code + template.builtins); close }
      s += input
    })

    return ast
  }

  def compile(code: String): Either[CompilationError, AST] = {
    for {
      tokens <- LipoLexer(code).right
      ast <- LipoParser(tokens).right
    } yield ast
  }

  def alreadyDone(input: String) : Boolean = {
    return s.contains(input)
  }
}

object Main extends App {
  val input = args(0)

  Compiler(input, Set[String]())
    .left.map(error => {
      println("\n")
      println(error)
      println("\n")
    })
}