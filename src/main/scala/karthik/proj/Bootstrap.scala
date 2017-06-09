package karthik.proj

import scala.io.StdIn
import karthik.proj.SimpleLambdaParser._

/**
  * Created by ksubramanian on 5/9/17.
  */
object Bootstrap extends App {
    val prompt = "LAMBDA>"
    val usage = "You can type in expression and press ENTER to evaluate a lambda expression or " +
        "you can type 'exit' to quit the REPL\nLanguage grammar: Use \\ in place of λ. Variables " +
        "have to be single character. Brackets are supported. Eg., (\\x.(\\x.x))y will evaluate " +
        "to \\x.x"
    println(usage)
    print(prompt);
    var line = ""
    while ({line = StdIn.readLine(); line != null}) {
        if (line.toLowerCase == "exit") {
            println("Exiting...")
            System.exit(0);
        } else {
            try {
                val le = NReduceEngine.execute(parse(line))
                printf("-> %s%n", le.toString);
            } catch {
                case evalEx : EvaluatorException => println(evalEx.getMessage)
                case ex : ParserException => println(ex.getMessage)
            };
            print(prompt)
        }
    }
}
