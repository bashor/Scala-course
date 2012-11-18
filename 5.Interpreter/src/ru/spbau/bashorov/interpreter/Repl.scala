package ru.spbau.bashorov.interpreter

object Repl {
  def main(args: Array[String]) {
    val evaluator = new MyEvaluator()
    var context = new Context()
    while (true) {
      readLine() match {
        case null => {
          println("Unexpected null.")
          return
        }
        case ":exit" => {
          println("goodbye")
          return
        }
        case ":reset" => {
          context = new Context()
        }
        case "" =>
        case input => {
          try {
            val result = evaluator.eval(input, context)
            result._1 match {
              case AstInt(value) => println(value)
              case AstDouble(value) => println(value)
              case _ => println(result)
            }
            context = result._2
          } catch {
            case e: RuntimeException => System.err.println(e.getMessage)
          }
        }
      }
    }
  }
}
