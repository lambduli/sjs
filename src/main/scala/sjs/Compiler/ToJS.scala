package compiler.tojs

import compiler._

import parser.ast._

object ToJSCompiler extends Compiler {
  def compile(root: AST): String = root match {
    case Program(content) => content.map(compile).mkString("\n")
    case Identifier(value) => s"$value"
    case Operator(value) => s"$value"
    case NumberLit(value) => s"$value"
    case StringLit(value) => s"""'$value'"""
    case BoolLit(value: Boolean) => s"$value"
    
    case LetExp(varlist: List[(String, AST)], body: AST) => {
      val args = varlist.map(_._1).mkString(", ")
      val values = varlist.map(_._2).map(compile).mkString(", ")
      val bod = compile(body)
      s"(($args) => $bod)($values)"
    }
    
    case AppList(value: List[AST]) => {
      val fn = compile(value.head)
      val args = value.tail.map(compile).mkString(", ")

      value(0) match {
        case Operator(op) => return s"(${compile(value(1))} $fn ${compile(value(2))})"
        case Identifier("cons") => s"[${compile(value(1))}, ...${compile(value(2))}]" // comment this line out if you want normal cons output
        case _ => return s"$fn($args)"
      }
    }
    
    case ValList(value: List[AST]) => {
      val values = value.map(compile).mkString(", ")
      s"[$values]"
    }
    
    case ArgList(value: List[AST]) => {
      val args = value.map(compile).mkString(", ")
      return s" ($args) "
    }
    
    case Import(ids: List[AST], path: String) => {
      val imported = ids.map(compile).mkString(", ")
      val ext = ".sjs".r
      val translated = ext.replaceAllIn(path, ".mjs")
      return s"import {$imported} from '$translated'\n"
    }

    case Export(id: String, expression: Option[AST]) =>
      expression match {
        case Some(expr) => return s"export const $id = ${compile(expr)}\n"
        case _ => return s"export { $id }\n"
      }

    case FnDef(name: String, args: AST, body: AST) => {
      val paramlist = compile(args)
      val defin = compile(body)
      return s"function $name $paramlist {\n  return$defin\n}\n"
    }

    case Lambda(args: AST, body: AST) => {
      val paramlist = compile(args)
      val defom = compile(body)
      return s"$paramlist=> ($defom) "
    }

    case Nil => "[]"

    case IfThenElse(condition: AST, `then`: AST, `else`: AST) => {
      val con = compile(condition)
      val thendo = compile(`then`)
      val otherwise = compile(`else`)
      return s" $con ? $thendo  :  $otherwise"
    }

    case Comment(content: String) => {
      val ext = ";;".r
      return ext.replaceAllIn(content, "//")
    }
  }

}