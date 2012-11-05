package ru.spbau.bashorov.interpreter

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import collection.mutable

class MyInterpreter extends Parser {
  def Digit = rule { "0" - "9" }
  def Digits = rule { oneOrMore(Digit) }
  def DoubleStr = rule { Digits ~ "." ~ Digits }

  def IntNum = rule { Digits ~> (s => new AstInt(s.toInt))}
  def DoubleNum = rule { DoubleStr ~> ((s: String) => new AstDouble(s.toDouble))}

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

  def Factor: Rule1[AstNode] = rule { Identifier | Number | UnOp | Parens}
  def Parens = rule { "(" ~ Expression ~ ")" }


  def IdentifierFirst = rule { "a"-"z" | "A" - "Z" | "_" }
  def IdentifierStr = rule { IdentifierFirst ~ zeroOrMore(IdentifierFirst | Digits)}

  def Identifier = rule { IdentifierStr ~> AstIdentifier}

  def Assignment = rule { Identifier ~ " = " ~ Expression ~~> ((id: AstIdentifier, value: AstNode) => AstAssignment(id, value)) }
  def FunCall = rule { Identifier ~ "(" ~ zeroOrMore(Identifier, separator = ", ") ~ ")" ~~>
    ((funName: AstIdentifier, params: List[AstNode]) => AstCall(funName, params)) }
  def CommaOp = rule (Expression ~ ", " ~ Expression ~~> ((f: AstNode, s:AstNode) => AstComma(f, s)))

  def Value = rule { "val " ~ Identifier ~ " = " ~ Expression ~~> ((name: AstIdentifier, expr: AstNode) => AstValue(name, expr))}
  def Variable = rule { "var " ~ Identifier ~ " = " ~ Expression ~~> ((name: AstIdentifier, expr: AstNode) => AstVariable(name, expr)) }

  class SignatureHolder(val name: AstIdentifier, val params: List[AstIdentifier])
  def FunSignature = rule { Identifier ~ "(" ~ zeroOrMore(Identifier, separator = ", ") ~ ")" ~~>
    ((name: AstIdentifier, params: List[AstIdentifier]) => new SignatureHolder(name, params))}
  def Function = rule { "def " ~ FunSignature ~ " = " ~ Expression ~~> ((fun: SignatureHolder, expr: AstNode) => AstFunction(fun.name, fun.params, expr)) }

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def Grammar = (Value | Variable | Function | Assignment | FunCall | CommaOp | Expression) ~ EOI

  def parse(expression: String) : AstNode = {
    val parsingResult = ReportingParseRunner(Grammar).run(expression)
//    val parsingResult = TracingParseRunner(Grammar).run(expression)

    parsingResult.result match {
      case Some(i) => i
      case None => throw new ParsingException("Invalid expression:\n" + ErrorUtils.printParseErrors(parsingResult))
    }
  }

  def unOp[T: Numeric](op: String, value: T) : T = op match {
    case "+" => value
    case "-" => implicitly[Numeric[T]].negate(value)
    case _ => throw new RuntimeException(s"Unknown unOp: $op")
  }

  private def binOp[T: Numeric](left: T, op: String, right: T, mapper: (T) => AstNode): AstNode = op match {
    case "+" => mapper(implicitly[Numeric[T]].plus(left, right))
    case "-" => mapper(implicitly[Numeric[T]].minus(left, right))
    case "*" => mapper(implicitly[Numeric[T]].times(left, right))
    case "/" => new AstDouble(implicitly[Numeric[T]].toDouble(left) / implicitly[Numeric[T]].toDouble(right))
    case _ => throw new RuntimeException(s"Unknown binOp: $op")
  }

  private def binOpWithExpr(expr: AstNode, op: String, other: AstNode, curContext: => mutable.Map[String, ContextElement]): AstNode = {
    eval(expr, mutable.Map() ++= curContext) match {
      case v@AstInt(_) => eval(AstBinOp(other, op, v), curContext)
      case v@AstDouble(_) => eval(AstBinOp(other, op, v), curContext)
      case _ => throw new RuntimeException("Bad expression")
    }
  }

  def eval(ast: AstNode, curContext: => mutable.Map[String, ContextElement]) : AstNode = ast match {
    case number @ AstInt(_) => number
    case number @ AstDouble(_) => number

    case AstIdentifier(name) => curContext.get(name).orNull match {
      case ContextValue(value) => if (value.isInstanceOf[Int]) AstInt(value.asInstanceOf[Int]) else AstDouble(value.asInstanceOf[Double])
      case ContextVariable(value) => if (value.isInstanceOf[Int]) AstInt(value.asInstanceOf[Int]) else AstDouble(value.asInstanceOf[Double])
      case ContextFunction(_, _) => throw new RuntimeException(s"Can not use Function name($name) as value")
      case _ => throw new RuntimeException("Unknown identefier")
    }

    case AstBinOp(AstInt(left), op, AstInt(right)) => binOp(left, op, right, AstInt)
    case AstBinOp(AstDouble(left), op, AstDouble(right)) => binOp(left, op, right, AstDouble)
    case AstBinOp(AstInt(left), op, AstDouble(right)) => binOp(left, op, right, AstDouble)
    case AstBinOp(AstDouble(left), op, AstInt(right)) => binOp(left, op, right, AstDouble)
    case AstBinOp(left @ AstInt(_), op, expr) => eval(AstBinOp(expr, op, left), curContext)
    case AstBinOp(left @ AstDouble(_), op, expr) => eval(AstBinOp(expr, op, left), curContext)
    case AstBinOp(expr, op, right) => binOpWithExpr(expr, op, right, curContext)

    case AstUnOp(op, AstInt(value)) => AstInt(unOp(op, value))
    case AstUnOp(op, AstDouble(value)) => AstDouble(unOp(op, value))
    case AstUnOp(op, expr) => eval(expr, mutable.Map() ++= curContext) match {
      case AstInt(value) => AstInt(unOp(op, value))
      case AstDouble(value) => AstDouble(unOp(op, value))
      case newExpr => AstUnOp(op, newExpr)
    }
    case ret @ AstAssignment(id, expr) => {
      if (!curContext.contains(id.name))
        throw new RuntimeException(s"Identifier ${id.name} not found")

      if (!curContext.get(id.name).get.isInstanceOf[ContextVariable[_]])
        throw new RuntimeException(s"Identifier ${id.name} is not variable")

      eval(expr, mutable.Map() ++= curContext) match {
        case AstInt(value) => curContext.put(id.name, ContextVariable(value))
        case AstDouble(value) => curContext.put(id.name, ContextVariable(value))
        case _ => throw new RuntimeException("Bad expression")
      }

      ret
    }
//    case AstCall(funName, params) =>
//    case AstComma(first, second) =>
    case ret @ AstFunction(id, params, body) => {
      if (curContext.contains(id.name))
        throw new RuntimeException(s"Identifier ${id.name} already exists")

      curContext.put(id.name, ContextFunction(params.map(_.name), body))

      ret
    }
    case ret @ AstValue(id, expr) => {
      if (curContext.contains(id.name))
        throw new RuntimeException(s"Identifier ${id.name} already exists")

      eval(expr, mutable.Map() ++= curContext) match {
        case AstInt(value) => curContext.put(id.name, ContextValue(value))
        case AstDouble(value) => curContext.put(id.name, ContextValue(value))
        case _ => throw new RuntimeException("Bad expression")
      }
      ret
    }
    case ret @ AstVariable(id, expr) => {
      if (curContext.contains(id.name))
        throw new RuntimeException(s"Identifier ${id.name} already exists")

      eval(expr, mutable.Map() ++= curContext) match {
        case AstInt(value) => curContext.put(id.name, ContextVariable(value))
        case AstDouble(value) => curContext.put(id.name, ContextVariable(value))
        case _ => throw new RuntimeException("Bad expression")
      }
      ret
    }
    case _ => throw new RuntimeException("Unknown AstNode")
  }

  def eval(expression: String): AstNode = {
    eval(parse(expression), context)
  }

  val context = new mutable.HashMap[String, ContextElement]
}

sealed abstract class ContextElement
case class ContextFunction(params: List[String], body: AstNode) extends  ContextElement
case class ContextValue[T](value: T) extends  ContextElement
case class ContextVariable[T](value: T) extends  ContextElement