package ru.spbau.bashorov.interpreter

sealed abstract class AstNode

case class AstInt(value: Int) extends AstNode

case class AstDouble(value: Double) extends AstNode

case class AstIdentifier(name: String) extends AstNode

case class AstBinOp(left: AstNode, op: String, right: AstNode) extends AstNode

case class AstUnOp(op: String, right: AstNode) extends AstNode

case class AstAssignment(id: AstIdentifier, value: AstNode) extends AstNode

case class AstCall(funName: AstIdentifier, params: List[AstNode]) extends AstNode

case class AstComma(first: AstNode, second: AstNode) extends AstNode


case class AstFunction(name: AstIdentifier, params: List[AstIdentifier], body: AstNode) extends AstNode

case class AstValue(name: AstIdentifier, expr: AstNode) extends AstNode

case class AstVariable(name: AstIdentifier, expr: AstNode) extends AstNode