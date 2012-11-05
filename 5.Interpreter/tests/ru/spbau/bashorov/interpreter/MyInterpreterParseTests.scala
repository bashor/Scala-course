package ru.spbau.bashorov.interpreter

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import org.parboiled.errors.ParsingException

@RunWith(classOf[JUnitRunner])
class MyInterpreterParseTests extends FunSuite {
  test("Simple ast tree") {
    new MyInterpreter().parse("1+2") should equal (AstBinOp(new AstInt(1), "+", new AstInt(2)))
  }

  test("Simple ast tree with doubles") {
    new MyInterpreter().parse("7.0-2.2") should equal (AstBinOp(new AstDouble(7.0), "-", new AstDouble(2.2)))
  }

  test("Mixed number") {
    new MyInterpreter().parse("433*2.2") should equal (AstBinOp(new AstInt(433), "*", new AstDouble(2.2)))
  }

  test("Error handling") {
    intercept[ParsingException] {
      new MyInterpreter().parse("1++")
    }
  }

  test("Parse Value") {
    new MyInterpreter().parse("val a = 7") should equal (AstValue(AstIdentifier("a"), new AstInt(7)))
  }

  test("BinOp with identifier") {
    new MyInterpreter().parse("abcd+27") should equal (AstBinOp(AstIdentifier("abcd"), "+", new AstInt(27)))
  }
}