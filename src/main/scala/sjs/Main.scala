package main

import java.io.PrintWriter
import scala.io.Source

import lexer._
import lexer.error._

import parser._
import parser.ast._

import template._

import compiler.tojs._
import compiler._

object Compilator {
  var s : Set[String] = _

  def apply(filePath: String, compiler: Compiler, $s : Set[String]): Either[CompilationError, AST] = {
    println(s"\n\nNow compiling file: $filePath\n\n")

    s = $s
    val source = Source.fromFile(filePath)
    val text = try source.getLines.mkString finally source.close
    val eitherAST = parseToAST(text)

    eitherAST.right.map(ast => {
      ast match {
        case Program(statements) => for (statement <- statements) {
          statement match {
            case Import(ids, path) => if (path.contains(".sjs") && ! alreadyDone(path)) {
              val prefix = "[.]/".r
              val trimmed = prefix.replaceAllIn(path, "")
              val lastSlash = filePath.lastIndexOf("/")
              val importFilePath = s"${filePath.substring(0, lastSlash)}/$trimmed"
              Compilator(importFilePath, compiler, s)
            }
            case _ => ()
          }
        }
        case _ => ()
      }
    })
    
    
    eitherAST.right.map(code => {
      val extPos = filePath.lastIndexOf(".sjs")
      val translatedFilePath = filePath.substring(0, extPos) ++ ".mjs"
      new PrintWriter(translatedFilePath) { write(compiler.compile(code) + template.builtins); close }
      s += filePath
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
  val currentDir = System.getProperty("user.dir")
  val filePath = s"$currentDir/$input"

  Compilator(filePath, ToJSCompiler, Set[String]())
    .left.map(error => {
      println("\n")
      println(error)
      println("\n")
    })
}