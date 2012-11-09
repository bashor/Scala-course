package ru.spbau.bashorov.interpreter

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import org.parboiled.errors.ParsingException

@RunWith(classOf[JUnitRunner])
class MyInterpreterParseTests extends FunSuite {
  test("Simple ast") {
    new MyInterpreter().parse("1+2") should equal (AstBinOp(new AstInt(1), "+", new AstInt(2)))
  }

  test("Simple ast with doubles") {
    new MyInterpreter().parse("7.0 - 2.2") should equal (AstBinOp(new AstDouble(7.0), "-", new AstDouble(2.2)))
  }

  test("Mixed number") {
    new MyInterpreter().parse("433 * 2.2") should equal (AstBinOp(new AstInt(433), "*", new AstDouble(2.2)))
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
    new MyInterpreter().parse("abcd + 27") should equal (AstBinOp(AstIdentifier("abcd"), "+", new AstInt(27)))
  }

  test("Parse comma") {
    new MyInterpreter().parse("1, 2, 3") should equal (AstComma(List(new AstInt(1), new AstInt(2), new AstInt(3))))
  }

  test("MaxInt") {
    new MyInterpreter().eval(Int.MaxValue.toString) should equal (new AstInt(Int.MaxValue))
  }

  test("MinInt") {
    new MyInterpreter().eval(Int.MinValue.toString) should equal (new AstInt(Int.MinValue))
  }

  test("Big Int") {
    intercept[RuntimeException] {
      new MyInterpreter().eval("123456789123456789")
    }
  }

  test("Max Double") {
    new MyInterpreter().eval(Double.MaxValue.formatted("%f")) should equal (new AstDouble(Double.MaxValue))
  }

  test("Min Double") {
    new MyInterpreter().eval(Double.MinValue.formatted("%f")) should equal (new AstDouble(Double.MinValue))
  }

  test("Big Double") {
    new MyInterpreter().eval("1" + Double.MaxValue.formatted("%f")) should equal (new AstDouble(Double.PositiveInfinity))
  }
}