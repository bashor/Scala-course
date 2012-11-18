package ru.spbau.bashorov.interpreter

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}

class MyParser extends Parser {
  def WhiteSpaces: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  def Digit = rule { "0" - "9" }
  def Digits = rule { oneOrMore(Digit) }
  def DoubleStr = rule { Digits ~ "." ~ Digits }

  def IntNum = rule { WhiteSpaces ~ (optional("-") ~ Digits) ~> (s => new AstInt(
      try {
        s.toInt
      } catch {
        case e: NumberFormatException => throw new ParsingException(s"Wrong number format: $s")
      })) ~ WhiteSpaces }
  def DoubleNum = rule { WhiteSpaces ~ (optional("-") ~ DoubleStr) ~> ((s: String) => new AstDouble(s.toDouble)) ~ WhiteSpaces }

  def Number = DoubleNum | IntNum

  def Expression = rule {
    Term ~ zeroOrMore (
      "+" ~ Term ~~> ((x: AstNode, y) => AstBinOp(x, "+", y).asInstanceOf[AstNode])
    | "-" ~ Term ~~> ((x: AstNode, y) => AstBinOp(x, "-", y).asInstanceOf[AstNode])
    )
  }
  def Term = rule {
    Factor ~ zeroOrMore(
      "*" ~ Factor ~~> ((x: AstNode, y) => AstBinOp(x, "*", y).asInstanceOf[AstNode])
    | "/" ~ Factor ~~> ((x: AstNode, y) => AstBinOp(x, "/", y).asInstanceOf[AstNode])
    )
  }
  def UnOp = rule {
    "-" ~ Expression ~~> ((expr: AstNode) => AstUnOp("-", expr).asInstanceOf[AstNode]) |
    "+" ~ Expression ~~> ((expr: AstNode) => AstUnOp("+", expr).asInstanceOf[AstNode])
  }
  def Factor: Rule1[AstNode] = rule { WhiteSpaces ~ (FunCall | Identifier | Number | UnOp | Parens) ~ WhiteSpaces}
  def Parens = rule { "(" ~ Expression ~ ")" }


  def IdentifierStart = rule { "a"-"z" | "A" - "Z" | "_" }
  def Identifier = rule { WhiteSpaces ~ (IdentifierStart ~ zeroOrMore(IdentifierStart | Digits)) ~> AstIdentifier ~ WhiteSpaces}

  def Assignment = rule { Identifier ~ "=" ~ Expression ~~> ((id: AstIdentifier, value: AstNode) => AstAssignment(id, value)) }

  def FunCall = rule { Identifier ~ "(" ~ zeroOrMore((Assignment | Expression), separator = ",") ~ ")" ~~>
    ((funName: AstIdentifier, params: List[AstNode]) => AstCall(funName, params)) }

  def Value = rule { "val" ~ Identifier ~ "=" ~ Expression ~~> ((name: AstIdentifier, expr: AstNode) => AstValue(name, expr))}

  def Variable = rule { "var" ~ Identifier ~ "=" ~ Expression ~~> ((name: AstIdentifier, expr: AstNode) => AstVariable(name, expr)) }

  def FunParam = rule { Assignment | Identifier }
  def FunSignature = rule { Identifier ~ "(" ~ zeroOrMore(FunParam, separator = ",") ~ optional("*") ~> ((s: String) => !s.isEmpty) ~ ")" ~ WhiteSpaces }
  def Function = rule { "def" ~ FunSignature ~ "=" ~ Operations ~~>
    ((name: AstIdentifier, params: List[AstNode], lastIsRepeated: Boolean, expr: AstNode) => AstFunction(name, params, lastIsRepeated, expr)) }

  def For = rule { "for" ~ WhiteSpaces ~ "(" ~ Identifier ~ ":" ~ Identifier ~")" ~ (Assignment | Expression) ~~>
    ((it: AstIdentifier, cont: AstIdentifier, body: AstNode) => AstFor(it, cont, body)) }

  def Operation = WhiteSpaces ~ (For | Value | Variable | Function | Assignment | Expression) ~ WhiteSpaces

  def Operations: Rule1[AstNode] = rule {oneOrMore(Operation, separator = ",") ~~>
    ((operations: List[AstNode]) => if (operations.size == 1) operations(0) else AstComma(operations)) }

  def Grammar = Operations ~ EOI

  def parse(expression: String) : AstNode = {
    val parsingResult = ReportingParseRunner(Grammar).run(expression)

    parsingResult.result match {
      case Some(i) => i
      case None => throw new ParsingException(ErrorUtils.printParseErrors(parsingResult))
    }
  }
}