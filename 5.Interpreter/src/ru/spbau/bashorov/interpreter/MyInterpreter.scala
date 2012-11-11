package ru.spbau.bashorov.interpreter

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import collection.mutable
import reflect.runtime.universe.TypeTag

class MyInterpreter extends Parser {
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
  def FunCall = rule { Identifier ~ "(" ~ zeroOrMore((Assignment | Expression), separator = ", ") ~ ")" ~~>
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
      case ContextFunction(_, _, _) => throw new RuntimeException(s"Can not use Function name($name) as value")
      case _ => throw new RuntimeException(s"Unknown identefier $name")
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
    case AstUnOp(op, expr) => eval(expr, curContext.clone()) match {
      case AstInt(value) => AstInt(unOp(op, value))
      case AstDouble(value) => AstDouble(unOp(op, value))
      case newExpr => AstUnOp(op, newExpr)
    }
    case ret @ AstAssignment(id, expr) => {
      if (!curContext.contains(id.name))
        throw new RuntimeException(s"Identifier ${id.name} not found")

      if (!curContext.get(id.name).get.isInstanceOf[ContextVariable[_]])
        throw new RuntimeException(s"Identifier ${id.name} is not variable")

      eval(expr, curContext.clone()) match {
        case AstInt(value) => curContext.put(id.name, ContextVariable(value))
        case AstDouble(value) => curContext.put(id.name, ContextVariable(value))
        case _ => throw new RuntimeException("Bad expression")
      }

      ret
    }
    case AstCall(astFun, params) => {
      val contextObject = curContext.get(astFun.name)
      if (contextObject.isEmpty)
        throw new RuntimeException(s"Function ${astFun.name} not found")

      if (!contextObject.get.isInstanceOf[ContextFunction])
        throw new RuntimeException(s"${astFun.name} is not Function")

      val fun = contextObject.get.asInstanceOf[ContextFunction]

//      val uninitedParams = mutable.MutableList[String]() ++= fun.params
      val initedParams = mutable.HashSet[String]()
      val newContext = curContext.clone()
      val repeatedParam = if (fun.lastIsRepeated) fun.params.last._1 else ""
      val repeatedVal = mutable.MutableList[AnyVal]()

      def addParam[T <: AnyVal : TypeTag](name: String, value: T) {
        if (name == repeatedParam) {
          repeatedVal += value
        } else {
          newContext.put(name, ContextValue[T](value))
          initedParams.add(name)
        }
      }

      for (i <- params.indices) {
        params(i) match {
          case AstAssignment(AstIdentifier(name), expr) => {
            if (fun.params.find(_._1 == name).isEmpty)
              throw new RuntimeException("Bad expression") //todo

            if (name != repeatedParam && initedParams.contains(name))
              throw new RuntimeException(s"Double initialization for parameter $name when calling ${astFun.name}")

            eval(expr, curContext.clone()) match {
              case AstInt(value) => addParam(name, value)
              case AstDouble(value) => addParam(name, value)
              case _ => throw new RuntimeException("Bad expression")
            }
          }
          case _ =>
        }
      }

      var j = 0
      for {param <- ((params).withFilter {
        case AstAssignment(_,_) => false
        case _ => true
      })} {
        while (j < fun.params.size && initedParams.contains(fun.params(j)._1)) j += 1
        if (j >= fun.params.size) {
          if (!repeatedParam.isEmpty)
            j = fun.params.size - 1
          else
            throw new RuntimeException(s"Incorrect parameters count for function ${astFun.name}")
        }


        eval(param, curContext.clone()) match {
          case AstInt(value) => addParam(fun.params(j)._1, value)
          case AstDouble(value) => addParam(fun.params(j)._1, value)
          case _ => throw new RuntimeException("Bad expression")
        }
      }

      for {param <- ((fun.params).withFilter { case (param) => !initedParams.contains(param._1) && param._2.isDefined })} {
        val name = param._1
        val value = param._2.get
        if (value.isInstanceOf[Int]) {
          addParam(name, value.asInstanceOf[Int])
        } else if (value.isInstanceOf[Double]) {
          addParam(name, value.asInstanceOf[Double])
        } else {
          //todo exception
        }
      }

      if (!repeatedParam.isEmpty) {
        newContext.put(repeatedParam, ContextList(repeatedVal.toList))
        initedParams.add(repeatedParam)
      }

      if (fun.params.size != initedParams.size)
        throw new RuntimeException(s"Incorrect parameters count for function ${astFun.name}")

      eval(fun.body, newContext)
    }

    case AstComma(expressions) => {
      for (expr <- expressions.take(expressions.size - 1)) {
        eval(expr, curContext)
       }
      eval(expressions.last, curContext)
    }


    case ret @ AstFunction(id, params, lastIsRepeated, body) => {
      if (curContext.contains(id.name))
        throw new RuntimeException(s"Identifier ${id.name} already exists")

      val funParams = params.map(_ match {
        case AstIdentifier(name) => (name, None)
        case AstAssignment(AstIdentifier(name), expr) => eval(expr, curContext.clone()) match {
          case AstInt(value) => (name, Some(value))
          case AstDouble(value) => (name, Some(value))
          case _ => throw new RuntimeException("todo") //todo
        }
        case _ => throw new RuntimeException("todo") //todo
      })

      for (i <- funParams.indices) {
        if (funParams.indexOf(funParams(i)) != i)
          throw new RuntimeException(s"Duplicate parameter name ${funParams(i)} in function ${id.name}")
      }

      curContext.put(id.name, ContextFunction(funParams, lastIsRepeated, body))

      ret
    }
    case ret @ AstValue(id, expr) => {
      if (curContext.contains(id.name))
        throw new RuntimeException(s"Identifier ${id.name} already exists")

      eval(expr, curContext.clone()) match {
        case AstInt(value) => curContext.put(id.name, ContextValue(value))
        case AstDouble(value) => curContext.put(id.name, ContextValue(value))
        case _ => throw new RuntimeException("Bad expression")
      }
      ret
    }
    case ret @ AstVariable(id, expr) => {
      if (curContext.contains(id.name))
        throw new RuntimeException(s"Identifier ${id.name} already exists")

      eval(expr, curContext.clone()) match {
        case AstInt(value) => curContext.put(id.name, ContextVariable(value))
        case AstDouble(value) => curContext.put(id.name, ContextVariable(value))
        case _ => throw new RuntimeException("Bad expression")
      }
      ret
    }

    case ret @ AstFor(it, cont, body) => {
      val contextObject = curContext.get(cont.name)
      if (contextObject.isEmpty)
        throw new RuntimeException(s"VarArg ${cont.name} not found")

      if (!contextObject.get.isInstanceOf[ContextList])
        throw new RuntimeException(s"${cont.name} is not VarArg")

      val vararg = contextObject.get.asInstanceOf[ContextList]

      for (arg <- vararg.list) {
        curContext.put(it.name, ContextValue(arg))
        eval(body, curContext)
      }
      curContext.remove(it.name)

      ret
    }
    case _ => throw new RuntimeException("Unknown AstNode")
  }

  def eval(expression: String): AstNode = {
    eval(parse(expression), context)
  }

  val context = new mutable.HashMap[String, ContextElement]
}

object MyInterpreter {
  val IntMinValueStr = Int.MinValue.toString
}

sealed abstract class ContextElement
case class ContextFunction(params: List[(String, Option[AnyVal])], lastIsRepeated: Boolean, body: AstNode) extends  ContextElement
case class ContextValue[T](value: T) extends  ContextElement
case class ContextVariable[T](value: T) extends  ContextElement
case class ContextList(list: List[AnyVal]) extends  ContextElement