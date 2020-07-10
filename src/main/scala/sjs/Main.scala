package main

import java.io.PrintWriter

import lexer._
import lexer.error._

import parser._
import parser.ast._

import template._

import compiler.tojs._
import compiler._

object Compilator {
  var s : Set[String] = _

  def apply(input: String, compiler: Compiler, $s : Set[String]): Either[CompilationError, AST] = {
    println(s"\n\nCompiling file: $input\n\n")

    s = $s
    val source = io.Source.fromInputStream(getClass.getResourceAsStream(s"/$input"))
    val text = try source.mkString finally source.close()
    val eitherAST = parseToAST(text)

    eitherAST.right.map(ast => {
      ast match {
        case Program(statements) => for (statement <- statements) {
          statement match {
            case Import(ids, path) => if (path.contains(".sjs") && ! alreadyDone(path)) {
              val prefix = "[.]/".r
              val trimmed = prefix.replaceAllIn(path, "")
              Compilator(trimmed, compiler, s)
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    })
    
    
    eitherAST.right.map(code => {
      val ext = ".sjs".r
      val translated = ext.replaceAllIn(input, ".mjs")
      new PrintWriter(translated) { write(compiler.compile(code) + template.builtins); close }
      s += input
    })

    return eitherAST
  }

  def parseToAST(code: String): Either[CompilationError, AST] = {
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

  Compilator(input, ToJSCompiler, Set[String]())
    .left.map(error => {
      println("\n")
      println(error)
      println("\n")
    })
}