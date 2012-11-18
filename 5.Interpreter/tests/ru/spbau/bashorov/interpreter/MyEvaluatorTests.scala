package ru.spbau.bashorov.interpreter

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

@RunWith(classOf[JUnitRunner])
class MyEvaluatorTests extends FunSuite {
  test("Int + Int") {
    new MyEvaluator().eval("123+234")._1 should equal (new AstInt(123+234))
  }

  test("Int / Int") {
    new MyEvaluator().eval("1/2")._1 should equal (new AstDouble(0.5))
  }

  test("Double - Int") {
    new MyEvaluator().eval("5.2-2")._1 should equal (new AstDouble(5.2 - 2))
  }

  test("Int * Double") {
    new MyEvaluator().eval("5*78.7")._1 should equal (new AstDouble(5 * 78.7))
  }

  test("Double / Double") {
    new MyEvaluator().eval("66.6/6.6")._1 should equal (new AstDouble(66.6 / 6.6))
  }

  test("Unary minus") {
    new MyEvaluator().eval("-2")._1 should equal (new AstInt(-2))
  }

  test("Unary plus") {
    new MyEvaluator().eval("+6.77")._1 should equal (new AstDouble(+6.77))
  }

  test("Create simple value and getting") {
    val evaluator = new MyEvaluator()

    val context = evaluator.eval("val a = 9")._2

    assert(context.contains("a"))

    evaluator.eval("a", context)._1 should equal(AstInt(9))
  }

  test("Create variable and getting") {
    val evaluator = new MyEvaluator()

    val context = evaluator.eval("var abcd = 98")._2

    assert(context.contains("abcd"))

    evaluator.eval("abcd", context)._1 should equal(AstInt(98))
  }

  test("Create variable with expression and getting with expression") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("var abcd = 9+435")._2
    val variable = context.get("abcd")
    assert(variable.isDefined)
    variable.get should equal (AstVariable(AstIdentifier("abcd"), AstInt(9 + 435)))

    evaluator.eval("abcd+34", context)._1 should equal(AstInt(9 + 435 + 34))
  }

  test("Double create value") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("val asd = 99.999")._2

    intercept[RuntimeException] {
      evaluator.eval("val asd = 10", context)
    }

    evaluator.eval("asd", context)._1 should equal(AstDouble(99.999))
  }

  test("Double create variable") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("var asd = 99.999")._2

    intercept[RuntimeException] {
      evaluator.eval("var asd = 10.222", context)
    }

    evaluator.eval("asd", context)._1 should equal(AstDouble(99.999))
  }

  test("Assign to variable") {
    val evaluator = new MyEvaluator()
    var context = evaluator.eval("var asd = 99.999")._2

    context = evaluator.eval("asd = 10+99.9", context)._2

    assert(context.contains("asd"))
    evaluator.eval("asd", context)._1 should equal (AstDouble(10 + 99.9))
  }

  test("Double assign to variable") {
    val evaluator = new MyEvaluator()
    var context = evaluator.eval("var asd = 99.999")._2

    context = evaluator.eval("asd = 10+99.9", context)._2

    context = evaluator.eval("asd = 0.9", context)._2

    assert(context.contains("asd"))
    evaluator.eval("asd", context)._1 should equal(AstDouble(0.9))
  }

  test("Assign to nonvariable") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("val asd = 99.999")._2

    intercept[RuntimeException] {
      evaluator.eval("asd = 10+99.9", context)
    }

    evaluator.eval("asd", context)._1 should equal(AstDouble(99.999))
  }

  test("Assign to nonexistent") {
    val evaluator = new MyEvaluator()

    intercept[RuntimeException] {
      evaluator.eval("asd = 10+99.9")
    }
  }

  test("Left is simple and right is expression") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("val abcd = 5")._2
    val variable = context.get("abcd")
    assert(variable.isDefined)
    variable.get should equal (AstInt(5))

    evaluator.eval("34+abcd", context)._1 should equal(AstInt(34 + 5))
  }

  test("Expression with whitespaces") {
    new MyEvaluator().eval("2 + 2")._1 should equal (new AstInt(4))
  }

  test("Function calling") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("def foo(x) = x")._2
    evaluator.eval("foo(3)", context)._1 should equal (AstInt(3))
  }

  test("Comma separated numbers") {
    new MyEvaluator().eval("1.1, 2, 3.7")._1 should equal (AstDouble(3.7))
  }

  test("Call function with named parameter") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("def foo(x, z) = x + z")._2
    evaluator.eval("foo(z = 1, 1) ", context)._1 should equal (AstInt(2))
  }

  test("Expression with function call") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("def foo() = 1 ")._2
    evaluator.eval(" 1 + foo()", context)._1 should equal (AstInt(2))
  }

  test("Define function with same params") {
    intercept[RuntimeException] {
      new MyEvaluator().eval("def foo(x, x) = 1")
    }
  }

  test("Double initialization for named parameter") {
    intercept[RuntimeException] {
      val evaluator = new MyEvaluator()
      val context = evaluator.eval("def foo(x, z) = x + z")._2
      evaluator.eval(" foo(z = 1, z = 1, 1) ", context)
    }
  }

  test("Incorrect parameter count") {
    intercept[RuntimeException] {
      val evaluator = new MyEvaluator()
      val context = evaluator.eval("def foo(x, z) = x + z")._2
      evaluator.eval(" foo(z = 1) ", context)
    }
  }

  test("Call function with few params") {
    val evaluator =  new MyEvaluator()
    val context = evaluator.eval("def foo(x, y, z) = x + y + z ")._2
    evaluator.eval(" foo(1, 2, 3.3)", context)._1 should equal (AstDouble(1 + 2 + 3.3))
  }

  test("Call function with excess param") {
    intercept[RuntimeException] {
      val evaluator = new MyEvaluator()
      val context = evaluator.eval("def foo(x, y, z) = x + y + z ")._2
      evaluator.eval(" foo(1, 2, 3.3, 4)", context)
    }
  }

  test("Call function with default param and named param and seq param") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("def foo(x, y = 2.2, z) = x + y + z ")._2
    evaluator.eval(" foo(z=3.3, 1.3)", context)._1 should equal (AstDouble(1.3 + 2.2 + 3.3))
  }

  test("Call function with named vararg param") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("def foo(x, y = 2.2, z*) = x + y")._2
    evaluator.eval(" foo(z=3.3, z = 3, 1.3)", context)._1 should equal (AstDouble(1.3 + 2.2))
  }

  test("for") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("def foo(z*) = var acc = 0, for (i : z) acc = acc + i, acc")._2
    evaluator.eval("foo(1, 2, 3)", context)._1 should equal (AstInt(1 + 2 + 3))
  }

  test("Call function many times") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("def foo(z*) = var acc = 0, for (i : z) acc = acc + i, acc")._2
    val (result, context2) = evaluator.eval("foo(1, 2, 3)", context)
    result should equal (AstInt(1 + 2 + 3))
    evaluator.eval("foo(z=1, z=2, 3)", context2)._1 should equal (AstInt(1 + 2 + 3))
  }

  test("Use variable name after used it in For loop") {
    val evaluator = new MyEvaluator()
    val context = evaluator.eval("def foo(z*) = var acc = 0, for (i : z) acc = acc + i, val i = acc, i")._2
    evaluator.eval("foo(z=1,z=2,3)", context)._1 should equal (AstInt(1 + 2 + 3))
  }

}
