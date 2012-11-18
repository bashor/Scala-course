package ru.spbau.bashorov.interpreter

trait AstNodeVisitor {
  def visited(node: AstInt, context: Context): (AstNode, Context)
  def visited(node: AstDouble, context: Context): (AstNode, Context)
  def visited(node: AstIdentifier, context: Context): (AstNode, Context)
  def visited(node: AstBinOp, context: Context): (AstNode, Context)
  def visited(node: AstUnOp, context: Context): (AstNode, Context)
  def visited(node: AstAssignment, context: Context): (AstNode, Context)
  def visited(node: AstCall, context: Context): (AstNode, Context)
  def visited(node: AstComma, context: Context): (AstNode, Context)
  def visited(node: AstFunction, context: Context): (AstNode, Context)
  def visited(node: AstValue, context: Context): (AstNode, Context)
  def visited(node: AstVariable, context: Context): (AstNode, Context)
  def visited(node: AstFor, context: Context): (AstNode, Context)
}