package ru.spbau.bashorov.interpreter

import collection.mutable
import org.parboiled.errors.ParsingException

sealed abstract class AstNode

case class AstInt(value: Int) extends AstNode

case class AstDouble(value: Double) extends AstNode

case class AstIdentifier(name: String) extends AstNode

case class AstBinOp(left: AstNode, op: String, right: AstNode) extends AstNode

case class AstUnOp(op: String, right: AstNode) extends AstNode

case class AstAssignment(id: AstIdentifier, value: AstNode) extends AstNode

case class AstCall(funName: AstIdentifier, params: List[AstNode]) extends AstNode

case class AstComma(expressions: List[AstNode]) extends AstNode

case class AstFunction(name: AstIdentifier, params: List[AstNode], lastIsRepeated: Boolean, body: AstNode) extends AstNode

case class AstValue(name: AstIdentifier, expr: AstNode) extends AstNode

case class AstVariable(name: AstIdentifier, expr: AstNode) extends AstNode

case class AstFor(it: AstIdentifier, collection: AstIdentifier, body: AstNode) extends  AstNode