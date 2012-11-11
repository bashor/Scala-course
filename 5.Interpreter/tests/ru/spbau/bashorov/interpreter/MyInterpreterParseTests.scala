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
    new MyInterpreter().parse(Int.MaxValue.toString) should equal (new AstInt(Int.MaxValue))
  }

  test("MinInt") {
    new MyInterpreter().parse(Int.MinValue.toString) should equal (new AstInt(Int.MinValue))
  }

  test("Big Int") {
    intercept[RuntimeException] {
      new MyInterpreter().parse("123456789123456789")
    }
  }

  test("Max Double") {
    new MyInterpreter().parse(Double.MaxValue.formatted("%f")) should equal (new AstDouble(Double.MaxValue))
  }

  test("Min Double") {
    new MyInterpreter().parse(Double.MinValue.formatted("%f")) should equal (new AstDouble(Double.MinValue))
  }

  test("Big Double") {
    new MyInterpreter().parse("1" + Double.MaxValue.formatted("%f")) should equal (new AstDouble(Double.PositiveInfinity))
  }

  test("Define function with default value") {
    new MyInterpreter().parse("def foo(x, y = 1, z) = 1") should equal (AstFunction(AstIdentifier("foo"), List(AstIdentifier("x"), AstAssignment(AstIdentifier("y"), AstInt(1)), AstIdentifier("z")), false, AstInt(1)))
  }

  test("Define function with repeated param") {
    new MyInterpreter().parse("def foo(x, z*) = 1") should equal (AstFunction(AstIdentifier("foo"), List(AstIdentifier("x"), AstIdentifier("z")), true, AstInt(1)))
  }

  test("Expression with many statement") {
    new MyInterpreter().parse("def foo() = 1 , 2") should equal (AstFunction(AstIdentifier("foo"), List(), false, AstComma(List(AstInt(1), AstInt(2)))))
  }

  test("Parse for") {
    new MyInterpreter().parse("for(i : z) i") should equal (AstFor(AstIdentifier("i"), AstIdentifier("z"), AstIdentifier("i")))
  }
}