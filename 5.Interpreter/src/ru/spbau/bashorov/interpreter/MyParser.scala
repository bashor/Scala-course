package ru.spbau.bashorov.interpreter

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}

class MyParser extends Parser {
  def Digit = rule { "0" - "9" }
  def Digits = rule { oneOrMore(Digit) }
  def DoubleStr = rule { Digits ~ "." ~ Digits }

  def IntNum = rule { WhiteSpace ~ (optional("-") ~ Digits) ~> (s => new AstInt(
      try {
        s.toInt
      } catch {
        case e: NumberFormatException => throw new RuntimeException(s"Too big number $s")
      })) ~ WhiteSpace }
  def DoubleNum = rule { WhiteSpace ~ (optional("-") ~ DoubleStr) ~> ((s: String) => new AstDouble(s.toDouble)) ~ WhiteSpace }

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

  def Factor: Rule1[AstNode] = rule { WhiteSpace ~ (FunCall | Identifier | Number | UnOp | Parens) ~ WhiteSpace}
  def Parens = rule { "(" ~ Expression ~ ")" }


  def IdentifierStart = rule { "a"-"z" | "A" - "Z" | "_" }
  def Identifier = rule { WhiteSpace ~ (IdentifierStart ~ zeroOrMore(IdentifierStart | Digits)) ~> AstIdentifier ~ WhiteSpace}

  def Assignment = rule { Identifier ~ "=" ~ Expression ~~> ((id: AstIdentifier, value: AstNode) => AstAssignment(id, value)) }
  def FunCall = rule { Identifier ~ "(" ~ zeroOrMore((Assignment | Expression), separator = ",") ~ ")" ~~>
    ((funName: AstIdentifier, params: List[AstNode]) => AstCall(funName, params)) }

  def Value = rule { "val" ~ Identifier ~ "=" ~ Expression ~~> ((name: AstIdentifier, expr: AstNode) => AstValue(name, expr))}
  def Variable = rule { "var" ~ Identifier ~ "=" ~ Expression ~~> ((name: AstIdentifier, expr: AstNode) => AstVariable(name, expr)) }

  class SignatureHolder(val name: AstIdentifier, val params: List[AstNode], val lastIsRepeated: Boolean)

  def FunParam = rule { Assignment | Identifier }
  def FunSignature = rule { Identifier ~ "(" ~ zeroOrMore(FunParam, separator = ",") ~ optional("*") ~> ((s: String) => !s.isEmpty) ~ ")" ~~>
    ((name: AstIdentifier, params: List[AstNode], lastIsRepeated: Boolean) => new SignatureHolder(name, params, lastIsRepeated)) ~ WhiteSpace }
  def Function = rule { "def" ~ FunSignature ~ "=" ~ CommaSeparated ~~>
    ((fun: SignatureHolder, expr: AstNode) => AstFunction(fun.name, fun.params, fun.lastIsRepeated, expr)) }

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def For = rule { "for" ~ WhiteSpace ~ "(" ~ Identifier ~ ":" ~ Identifier ~")" ~ (Assignment | Expression) ~~>
    ((it: AstIdentifier, cont: AstIdentifier, body: AstNode) => AstFor(it, cont, body)) }

  def Operation = WhiteSpace ~ (For | Value | Variable | Function | Assignment | Expression) ~ WhiteSpace
  def CommaSeparated: Rule1[AstNode] = rule {oneOrMore(Operation, separator = ",") ~~>
    ((operations: List[AstNode]) => if (operations.size == 1) operations(0) else AstComma(operations)) }
  def Grammar = CommaSeparated ~ EOI

  def parse(expression: String) : AstNode = {
    val parsingResult = ReportingParseRunner(Grammar).run(expression)
//    val parsingResult = TracingParseRunner(Grammar).run(expression)

    parsingResult.result match {
      case Some(i) => i
      case None => throw new ParsingException("Invalid expression:\n" + ErrorUtils.printParseErrors(parsingResult))
    }
  }
}