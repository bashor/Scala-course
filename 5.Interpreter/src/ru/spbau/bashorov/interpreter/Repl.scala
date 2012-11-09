package ru.spbau.bashorov.interpreter

object Repl {
  def main(args: Array[String]) {
    var myInterpreter = new MyInterpreter
    while (true) {
      readLine() match {
        case null => {
          println("bedabeda")
          return
        }
        case ":exit" => {
          println("goodbye")
          return
        }
        case ":reset" => {
          myInterpreter = new MyInterpreter
        }
        case input => {
          try {
            val result = myInterpreter.eval(input)
            result match {
              case AstInt(value) => println(value)
              case AstDouble(value) => println(value)
              case _ => println(result)
            }
          } catch {
            case e: RuntimeException => System.err.println(e.getMessage)
          }
        }
      }
    }
  }
}
