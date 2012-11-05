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
            println(myInterpreter.eval(input))
          } catch {
            case e: RuntimeException => System.err.println(e.getMessage)
          }
        }
      }
    }
  }
}
