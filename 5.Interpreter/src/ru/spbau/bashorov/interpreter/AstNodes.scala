package ru.spbau.bashorov.interpreter

sealed abstract class AstNode {
  def visit(visitor: AstNodeVisitor, context: Context): (AstNode, Context)
}

case class AstInt(value: Int) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstDouble(value: Double) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstIdentifier(name: String) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstBinOp(left: AstNode, op: String, right: AstNode) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstUnOp(op: String, right: AstNode) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstAssignment(id: AstIdentifier, expr: AstNode) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstCall(funId: AstIdentifier, params: List[AstNode]) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstComma(expressions: List[AstNode]) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstFunction(id: AstIdentifier, params: List[AstNode], lastIsRepeated: Boolean, body: AstNode) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstValue(id: AstIdentifier, expr: AstNode) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstVariable(id: AstIdentifier, expr: AstNode) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstFor(it: AstIdentifier, collection: AstIdentifier, body: AstNode) extends  AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = visitor.visited(this, context)
}

case class AstVarArgs(args: List[AstNode]) extends AstNode {
  def visit(visitor: AstNodeVisitor, context: Context) = (this, context)
}
