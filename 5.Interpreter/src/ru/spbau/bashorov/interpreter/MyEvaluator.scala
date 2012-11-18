package ru.spbau.bashorov.interpreter

import collection.mutable

class MyEvaluator extends AstNodeVisitor {

  def visited(node: AstInt, context: Context) = (node, context)

  def visited(node: AstDouble, context: Context) = (node, context)

  def visited(node: AstIdentifier, context: Context) = context.get(node.name).orNull match {
    case num @ AstInt(_) => (num, context)
    case num @ AstDouble(_) => (num, context)
    case AstValue(_, expr) => expr.visit(this, context)
    case AstVariable(_, expr) => expr.visit(this, context)
    case AstFunction(_, _, _, _) => throw new RuntimeException(s"Can not use Function name(${node.name}) as expr")
    case _ => throw new RuntimeException(s"Unknown identefier ${node.name}")
  }

  private def binOp[T: Numeric](left: T, op: String, right: T, mapper: (T) => AstNode): AstNode = op match {
    case "+" => mapper(implicitly[Numeric[T]].plus(left, right))
    case "-" => mapper(implicitly[Numeric[T]].minus(left, right))
    case "*" => mapper(implicitly[Numeric[T]].times(left, right))
    case "/" => new AstDouble(implicitly[Numeric[T]].toDouble(left) / implicitly[Numeric[T]].toDouble(right))
    case _ => throw new RuntimeException(s"Unknown binOp: $op")
  }

  def visited(node: AstBinOp, context: Context) = node match {
    case AstBinOp(AstInt(left), op, AstInt(right)) => (binOp(left, op, right, AstInt), context)
    case AstBinOp(AstDouble(left), op, AstDouble(right)) => (binOp(left, op, right, AstDouble), context)
    case AstBinOp(AstInt(left), op, AstDouble(right)) => (binOp(left, op, right, AstDouble), context)
    case AstBinOp(AstDouble(left), op, AstInt(right)) => (binOp(left, op, right, AstDouble), context)
    case AstBinOp(expr, op, right) => {
      processNum(expr.visit(this, context)._1, ((num: AstNode) => (AstBinOp(right, op, num).visit(this, context)._1, context)))
    }
  }

  def unOp[T: Numeric](op: String, value: T) : T = op match {
    case "+" => value
    case "-" => implicitly[Numeric[T]].negate(value)
    case _ => throw new RuntimeException(s"Unknown unOp: $op")
  }

  def visited(node: AstUnOp, context: Context) = node match {
    case AstUnOp(op, AstInt(value)) => (AstInt(unOp(op, value)), context)
    case AstUnOp(op, AstDouble(value)) => (AstDouble(unOp(op, value)), context)
    case AstUnOp(op, expr) => expr.visit(this, context) match {
      case (AstInt(value), _) => (AstInt(unOp(op, value)), context)
      case (AstDouble(value), _) => (AstDouble(unOp(op, value)), context)
      case (newExpr, _) => (AstUnOp(op, newExpr), context)
    }
  }

  def visited(node: AstAssignment, context: Context) = {
    if (!context.contains(node.id.name))
      throw new RuntimeException(s"Identifier ${node.id.name} not found")

    if (!context.get(node.id.name).get.isInstanceOf[AstVariable])
      throw new RuntimeException(s"Identifier ${node.id.name} is not variable")

    val result = node.expr.visit(this, context)._1

    (result, processNum(node.expr.visit(this, context)._1, ((num:AstNode) => context ++ (node.id.name, AstVariable(node.id, num)))))
  }

  def visited(node: AstComma, context: Context) = {
    var result = (node: AstNode, context)
    for (expr <- node.expressions) {
      result = expr.visit(this, result._2)
    }
    result
  }

  def visited(node: AstValue, context: Context) = {
    if (context.contains(node.id.name))
      throw new RuntimeException(s"Identifier ${node.id.name} already exists")

    val result = node.expr.visit(this, context)._1
    (result, processNum(result,((num:AstNode) => context ++ (node.id.name, num))))
  }

  def visited(node: AstVariable, context: Context) = {
    if (context.contains(node.id.name))
      throw new RuntimeException(s"Identifier ${node.id.name} already exists")

    val result = node.expr.visit(this, context)._1
    (result, processNum(result,((num:AstNode) => context ++ (node.id.name, AstVariable(node.id, num)))))
  }

  def visited(node: AstFor, context: Context) = {
    val contextObject = context.get(node.collection.name)
    if (contextObject.isEmpty)
      throw new RuntimeException(s"VarArg ${node.collection.name} not found")

    if (!contextObject.get.isInstanceOf[AstVarArgs])
      throw new RuntimeException(s"${node.collection.name} is not VarArgs")

    val varargs = contextObject.get.asInstanceOf[AstVarArgs]

    var result = (node: AstNode, context)
    for (arg <- varargs.args) {
      result = node.body.visit(this, result._2 ++ (node.it.name, arg))
    }

    (result._1, (result._2 -- node.it.name))
  }

  def visited(node: AstFunction, context: Context) = {
    if (context.contains(node.id.name))
      throw new RuntimeException(s"Identifier ${node.id.name} already exists")

    val funParams = node.params.map(_ match {
      case id @ AstIdentifier(_) => id
      case AstAssignment(id, expr) => processNum(expr.visit(this, context)._1, AstAssignment(id, _))
      case param => throw new RuntimeException(s"Unexpected node in function params: $param")
    })

    for (i <- funParams.indices) {
      if (funParams.indexOf(funParams(i)) != i)
        throw new RuntimeException(s"Duplicate parameter name ${funParams(i)} in function ${node.id.name}")
    }

    val newContext = context ++ (node.id.name, AstFunction(node.id, funParams, node.lastIsRepeated, node.body))

    (node, newContext)
  }

  def visited(node: AstCall, context: Context) = {
    def extractName(node: AstNode) = node match {
      case id @ AstIdentifier(_) => id.name
      case AstAssignment(id, _) =>  id.name
      case _ => throw new RuntimeException(s"Unexpected node in function params: $node")
    }

    def extractValue(node: AstNode) = node match {
      case id @ AstIdentifier(_) => None
      case AstAssignment(id, value) =>  Some(value)
      case _ => throw new RuntimeException(s"Unexpected node in function params: $node")
    }

    val contextObject = context.get(node.funId.name)
    if (contextObject.isEmpty)
      throw new RuntimeException(s"Function ${node.funId.name} not found")

    if (!contextObject.get.isInstanceOf[AstFunction])
      throw new RuntimeException(s"${node.funId.name} is not Function")

    val fun = contextObject.get.asInstanceOf[AstFunction]

    val initedParams = mutable.HashSet[String]()
    var newContext = mutable.HashMap[String, AstNode]()
    val repeatedVal = mutable.MutableList[AstNode]()
    val repeatedParam = if (fun.lastIsRepeated) extractName(fun.params.last) else ""

    def addParam(name: String, value: AstNode) {
      if (name == repeatedParam) {
        repeatedVal += value
      } else {
        newContext.put(name, value)
        initedParams.add(name)
      }
    }

    // init all named parameters
    for (i <- node.params.indices) {
      node.params(i) match {
        case AstAssignment(AstIdentifier(name), expr) => {
          val param = fun.params.find(((node: AstNode) => extractName(node) == name))
          if (param.isEmpty)
            throw new RuntimeException(s"Parameter $name not found in function ${fun.id.name}")

          if (name != repeatedParam && initedParams.contains(name))
            throw new RuntimeException(s"Double initialization for parameter $name when calling ${node.funId.name}")

          processNum(expr.visit(this, context)._1, addParam(name, _))
        }
        case _ =>
      }
    }

    // init another parameters
    var j = 0
    for {param <- ((node.params).withFilter {
      case AstAssignment(_,_) => false
      case _ => true
    })} {
      while (j < fun.params.size && initedParams.contains(extractName(fun.params(j)))) j += 1
      if (j >= fun.params.size) {
        if (!repeatedParam.isEmpty)
          j = fun.params.size - 1
        else
          throw new RuntimeException(s"Incorrect parameters count for function ${node.funId.name}")
      }

      val paramName = extractName(fun.params(j))
      processNum(param.visit(this, context)._1, addParam(paramName, _))
    }

    //init default parameters
    for {param <- ((fun.params).withFilter { case (param) => !initedParams.contains(extractName(param)) && extractValue(param).isDefined })} {
      val name = extractName(param)
      val value = extractValue(param).get
      addParam(name, value)
    }

    // init varargs
    if (!repeatedParam.isEmpty) {
      newContext.put(repeatedParam, AstVarArgs(repeatedVal.toList))
      initedParams.add(repeatedParam)
    }

    if (fun.params.size != initedParams.size)
      throw new RuntimeException(s"Incorrect parameters count for function ${node.funId.name}")

    (eval(fun.body, context ++ newContext.toMap)._1, context)
  }

  private def processNum[R](node: AstNode, process: (AstNode) => R) = node match {
    case num @ AstInt(_) => process(num)
    case num @ AstDouble(_) => process(num)
    case _ => throw new RuntimeException("It is not Num")
  }


  val parser = new MyParser()

  def eval(node: AstNode, context: Context): (AstNode, Context) = node.visit(this, context)
  def eval(expression: String, context: Context = new Context(Map())): (AstNode, Context) = eval(parser.parse(expression), context)
}
