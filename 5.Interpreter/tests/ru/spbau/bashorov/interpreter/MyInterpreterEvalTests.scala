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

  test("Left is simple and right is expression") {
    val interpreter = new MyInterpreter()
    interpreter.eval("val abcd = 5")
    val variable = interpreter.context.get("abcd")
    assert(variable.isDefined)
    variable.get should equal (ContextValue(5))

    interpreter.eval("34+abcd") should equal(AstInt(34 + 5))
  }

  test("Expression with whitespaces") {
    new MyInterpreter().eval("2 + 2") should equal (new AstInt(4))
  }

  test("Function calling") {
    val interpreter = new MyInterpreter()
    interpreter.eval("def foo(x) = x")
    interpreter.eval("foo(3)") should equal (AstInt(3))
  }

  test("Comma separated numbers") {
    new MyInterpreter().eval("1.1, 2, 3.7") should equal (AstDouble(3.7))
  }

  test("Call function with named parameter") {
    val interpreter = new MyInterpreter()
    interpreter.eval("def foo(x, z) = x + z")
    interpreter.eval("foo(z = 1, 1) ") should equal (AstInt(2))
  }

  test("Expression with function call") {
    val interpreter = new MyInterpreter()
    interpreter.eval("def foo() = 1 ")
    interpreter.eval(" 1 + foo()") should equal (AstInt(2))
  }

  test("Define function with same params") {
    intercept[RuntimeException] {
      new MyInterpreter().eval("def foo(x, x) = 1")
    }
  }

  test("Double initialization for named parameter") {
    intercept[RuntimeException] {
      val interpreter = new MyInterpreter()
      interpreter.eval("def foo(x, z) = x + z")
      interpreter.eval(" foo(z = 1, z = 1, 1) ")
    }
  }

  test("Incorrect parameter count") {
    intercept[RuntimeException] {
      val interpreter = new MyInterpreter()
      interpreter.eval("def foo(x, z) = x + z")
      interpreter.eval(" foo(z = 1) ")
    }
  }

  test("Call function with few params") {
    val interpreter =  new MyInterpreter()
    interpreter.eval("def foo(x, y, z) = x + y + z ")
    interpreter.eval(" foo(1, 2, 3.3)") should equal (AstDouble(1 + 2 + 3.3))
  }

  test("Call function with excess param") {
    intercept[RuntimeException] {
      val interpreter = new MyInterpreter()
      interpreter.eval("def foo(x, y, z) = x + y + z ")
      interpreter.eval(" foo(1, 2, 3.3, 4)")
    }
  }

  test("Call function with default param and named param and seq param") {
    val interpreter = new MyInterpreter()
    interpreter.eval("def foo(x, y = 2.2, z) = x + y + z ")
    interpreter.eval(" foo(z=3.3, 1.3)") should equal (AstDouble(1.3 + 2.2 + 3.3))
  }

  test("Call function with named vararg param") {
    val interpreter = new MyInterpreter()
    interpreter.eval("def foo(x, y = 2.2, z*) = x + y")
    interpreter.eval(" foo(z=3.3, z = 3, 1.3)") should equal (AstDouble(1.3 + 2.2))
  }

  test("for") {
    val interpreter = new MyInterpreter()
    interpreter.eval("def foo(z*) = var acc = 0, for (i : z) acc = acc + i, acc")
    interpreter.eval("foo(1, 2, 3)") should equal (AstInt(1 + 2 + 3))
  }

}
