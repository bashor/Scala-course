package ru.spbau.bashorov.interpreter

import collection.mutable

class Context(private val map: Map[String, AstNode] = Map()) {
  def get(key: String) = map.get(key)

  def contains(key: String) = map.contains(key)

  def ++(newElements: Map[String, AstNode]) = {
    val newMap = mutable.HashMap(map.toSeq: _*) ++ newElements
    new Context(newMap.toMap)
  }

  def ++(newElement: (String, AstNode)) = {
    val newMap = mutable.HashMap(map.toSeq: _*)
    newMap += newElement
    new Context(newMap.toMap)
  }

  def -- (key: String) = {
    val newMap = mutable.HashMap(map.toSeq: _*)
    newMap.remove(key)
    new Context(newMap.toMap)
  }
}