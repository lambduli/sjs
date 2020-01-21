package parser.ast

import lexer.token._

sealed trait AST

case class Program(content: List[AST]) extends AST {
  override def toString = content.map(_.toString).mkString("\n")
}

case class Identifier(value: String) extends AST {
  override def toString = s"$value"
}
case class Operator(value: Token) extends AST {
  override def toString = s"$value"
}

case class NumberLit(value: Number) extends AST {
  override def toString = s"$value"
}
case class StringLit(value: String) extends AST {
  override def toString = s"""'$value'"""
}
case class BoolLit(value: Boolean) extends AST {
  override def toString = s"$value"
}

case class AppList(value: List[AST]) extends AST {
  override def toString: String = {
    val fn = value.head.toString
    val args = value.tail.map(_.toString).mkString(", ")

    value(0) match {
      case Operator(op) => return s"(${value(1)} $fn ${value(2)})"
      case Identifier("cons") => s"[${value(1)}, ...${value(2)}]" // comment this line out if you want normal cons output
      case _ => return s"$fn($args)"
    }
  }
}
case class ValList(value: List[AST]) extends AST {
  override def toString: String = {
    val values = value.map(_.toString).mkString(", ")
    return s"[$values]"
  }
}
case class ArgList(value: List[AST]) extends AST {
  override def toString: String = {
    val args = value.map(_.toString).mkString(", ")
    return s" ($args) "
  }
}

case class Import(ids: List[AST], path: String) extends AST {
  override def toString: String = {
    val imported = ids.map(_.toString).mkString(", ")
    val ext = ".sjs".r
    val translated = ext.replaceAllIn(path, ".mjs")
    return s"import {$imported} from '$translated'"
  }
}

case class Export(id: String, expression: Option[AST]) extends AST {
  override def toString: String = {
    expression match {
      case Some(expr) => return s"export const $id = $expr"
      case _ => return s"export { $id }"
    }
  }
}

case class FnDef(name: String, args: AST, body: AST) extends AST {
  override def toString: String = {
    val paramlist = args.toString
    val defin = body.toString
    return s"function $name $paramlist {\n  return$defin\n}\n"
  }
}
case class Lambda(args: AST, body: AST) extends AST {
  override def toString: String = {
    val paramlist = args.toString
    val defom = body.toString
    return s"$paramlist=> ($defom) "
  }
}
case object Nil extends AST {
  override def toString = "[]"
}

case class IfThenElse(condition: AST, thenDo: AST, otherwise: AST) extends AST {
  override def toString: String = {
    val con = condition.toString
    val thendo = thenDo.toString
    val other = otherwise.toString
    return s" $con ? $thendo  :  $other"
  }
}

case class Comment(content: String) extends AST {
  override def toString: String = {
    val ext = ";;".r
    return ext.replaceAllIn(content, "//")
  }
}