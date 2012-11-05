package ru.spbau.bashorov.interpreter

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._


@RunWith(classOf[JUnitRunner])
class MyInterpreterEvalTests extends FunSuite {
  test("Int + Int") {
    new MyInterpreter().eval("123+234") should equal (new AstInt(123+234))
  }

  test("Int / Int") {
    new MyInterpreter().eval("1/2") should equal (new AstDouble(0.5))
  }

  test("Double - Int") {
    new MyInterpreter().eval("5.2-2") should equal (new AstDouble(5.2 - 2))
  }

  test("Int * Double") {
    new MyInterpreter().eval("5*78.7") should equal (new AstDouble(5 * 78.7))
  }

  test("Double / Double") {
    new MyInterpreter().eval("66.6/6.6") should equal (new AstDouble(66.6 / 6.6))
  }

  test("Unary minus") {
    new MyInterpreter().eval("-2") should equal (new AstInt(-2))
  }

  test("Unary plus") {
    new MyInterpreter().eval("+6.77") should equal (new AstDouble(+6.77))
  }

  test("Create simple value and getting") {
    val interpreter = new MyInterpreter()
    interpreter.eval("val a = 9")
    assert(interpreter.context.contains("a"))

    interpreter.eval("a") should equal(AstInt(9))
  }

  test("Create variable and getting") {
    val interpreter = new MyInterpreter()
    interpreter.eval("var abcd = 98")
    assert(interpreter.context.contains("abcd"))

    interpreter.eval("abcd") should equal(AstInt(98))
  }

  test("Create variable with expression and getting with expression") {
    val interpreter = new MyInterpreter()
    interpreter.eval("var abcd = 9+435")
    val variable = interpreter.context.get("abcd")
    assert(variable.isDefined)
    variable.get should equal (ContextVariable(9 + 435))

    interpreter.eval("abcd+34") should equal(AstInt(9 + 435 + 34))
  }

  test("Double create value") {
    val interpreter = new MyInterpreter()
    interpreter.eval("val asd = 99.999")

    intercept[RuntimeException] {
      interpreter.eval("val asd = 10")
    }

    assert(interpreter.context.contains("asd"))
    interpreter.eval("asd") should equal(AstDouble(99.999))
  }

  test("Double create variable") {
    val interpreter = new MyInterpreter()
    interpreter.eval("var asd = 99.999")

    intercept[RuntimeException] {
      interpreter.eval("var asd = 10.222")
    }

    assert(interpreter.context.contains("asd"))
    interpreter.eval("asd") should equal(AstDouble(99.999))
  }

  test("Assign to variable") {
    val interpreter = new MyInterpreter()
    interpreter.eval("var asd = 99.999")

    interpreter.eval("asd = 10+99.9")

    assert(interpreter.context.contains("asd"))
    interpreter.eval("asd") should equal (AstDouble(10 + 99.9))
  }

  test("Double assign to variable") {
    val interpreter = new MyInterpreter()
    interpreter.eval("var asd = 99.999")

    interpreter.eval("asd = 10+99.9")

    interpreter.eval("asd = 0.9")

    assert(interpreter.context.contains("asd"))
    interpreter.eval("asd") should equal(AstDouble(0.9))
  }

  test("Assign to nonvariable") {
    val interpreter = new MyInterpreter()
    interpreter.eval("val asd = 99.999")

    intercept[RuntimeException] {
      interpreter.eval("asd = 10+99.9")
    }

    assert(interpreter.context.contains("asd"))
    interpreter.eval("asd") should equal(AstDouble(99.999))
  }

  test("Assign to nonexistent") {
    val interpreter = new MyInterpreter()

    intercept[RuntimeException] {
      interpreter.eval("asd = 10+99.9")
    }
  }

  test("left is simple and right is expression ") {
    val interpreter = new MyInterpreter()
    interpreter.eval("val abcd = 5")
    val variable = interpreter.context.get("abcd")
    assert(variable.isDefined)
    variable.get should equal (ContextValue(5))

    interpreter.eval("34+abcd") should equal(AstInt(34 + 5))
  }

}
