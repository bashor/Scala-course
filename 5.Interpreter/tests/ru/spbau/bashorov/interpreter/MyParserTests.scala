package ru.spbau.bashorov.interpreter

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import org.parboiled.errors.ParsingException

@RunWith(classOf[JUnitRunner])
class MyParserTests extends FunSuite {
  test("Simple ast") {
    new MyParser().parse("1+2") should equal (AstBinOp(new AstInt(1), "+", new AstInt(2)))
  }

  test("Simple ast with doubles") {
    new MyParser().parse("7.0 - 2.2") should equal (AstBinOp(new AstDouble(7.0), "-", new AstDouble(2.2)))
  }

  test("Mixed number") {
    new MyParser().parse("433 * 2.2") should equal (AstBinOp(new AstInt(433), "*", new AstDouble(2.2)))
  }

  test("Error handling") {
    intercept[ParsingException] {
      new MyParser().parse("1++")
    }
  }

  test("Parse Value") {
    new MyParser().parse("val a = 7") should equal (AstValue(AstIdentifier("a"), new AstInt(7)))
  }

  test("BinOp with identifier") {
    new MyParser().parse("abcd + 27") should equal (AstBinOp(AstIdentifier("abcd"), "+", new AstInt(27)))
  }

  test("Parse comma") {
    new MyParser().parse("1, 2, 3") should equal (AstComma(List(new AstInt(1), new AstInt(2), new AstInt(3))))
  }

  test("MaxInt") {
    new MyParser().parse(Int.MaxValue.toString) should equal (new AstInt(Int.MaxValue))
  }

  test("MinInt") {
    new MyParser().parse(Int.MinValue.toString) should equal (new AstInt(Int.MinValue))
  }

  test("Big Int") {
    intercept[RuntimeException] {
      new MyParser().parse("123456789123456789")
    }
  }

  test("Max Double") {
    new MyParser().parse(Double.MaxValue.formatted("%f")) should equal (new AstDouble(Double.MaxValue))
  }

  test("Min Double") {
    new MyParser().parse(Double.MinValue.formatted("%f")) should equal (new AstDouble(Double.MinValue))
  }

  test("Big Double") {
    new MyParser().parse("1" + Double.MaxValue.formatted("%f")) should equal (new AstDouble(Double.PositiveInfinity))
  }

  test("Define function with default value") {
    new MyParser().parse("def foo(x, y = 1, z) = 1") should equal (AstFunction(AstIdentifier("foo"), List(AstIdentifier("x"), AstAssignment(AstIdentifier("y"), AstInt(1)), AstIdentifier("z")), false, AstInt(1)))
  }

  test("Define function with repeated param") {
    new MyParser().parse("def foo(x, z*) = 1") should equal (AstFunction(AstIdentifier("foo"), List(AstIdentifier("x"), AstIdentifier("z")), true, AstInt(1)))
  }

  test("Expression with many statement") {
    new MyParser().parse("def foo() = 1 , 2") should equal (AstFunction(AstIdentifier("foo"), List(), false, AstComma(List(AstInt(1), AstInt(2)))))
  }

  test("Parse for") {
    new MyParser().parse("for(i : z) i") should equal (AstFor(AstIdentifier("i"), AstIdentifier("z"), AstIdentifier("i")))
  }
}