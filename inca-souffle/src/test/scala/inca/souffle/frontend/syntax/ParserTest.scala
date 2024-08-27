package inca.souffle.frontend.syntax

import inca.souffle.syntax.Parser
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite:

  test("oneOperator") {
    Parser.binop.parseAll("+").getOrElse(???)
    Parser.binop.parseAll("lxor").getOrElse(???)
  }

  test("directive") {
    Parser.directive.parseAll(".input foo").getOrElse(???)
    Parser.directive.parseAll(".input DirectSuperclass(IO=\"file\", filename=\"DirectSuperclass.facts\", delimiter=\"\\t\")").getOrElse(???)
  }

  test("terms") {
    Parser.varidentifier.parseAll("?returnType").getOrElse(???)
    Parser.varidentifier.parseAll("returnType").getOrElse(???)
    Parser.term.parseAll("?returnType").getOrElse(???)
    Parser.term.parseAll("\"analysis\"").getOrElse(???)
    Parser.intrinsicFunctor.parseAll("cat").getOrElse(???)
    Parser.term.parseAll("cat(?returnType, ?a)").getOrElse(???)
    Parser.term.parseAll("cat(?returnType, cat(\"(\", cat(?params, \")\")))").getOrElse(???)
  }

  test("component") {
    Parser.component.parseAll(".comp Basic {}").getOrElse(???)
  }

  test("query plan") {
    Parser.plan.parseAll(".plan 1:(3,2,1)").getOrElse(???)
    Parser.plan.parseAll(".plan 1 : (3,2,1)").getOrElse(???)
  }
