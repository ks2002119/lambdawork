package karthik.proj


import java.util.Calendar
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap, Executors, ThreadFactory}

import akka.util.Timeout
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import org.backuity.ansi.AnsiFormatter.FormattedHelper
/**
  * Created by ksubramanian on 6/3/17.
  */
object NReduceEngine {
    val log = LoggerFactory.getLogger("NReduceEngine")
    val random = new scala.util.Random(31)
    val lock = new ReentrantLock()
    val colors : ConcurrentMap[Long, String] = new ConcurrentHashMap()
    private val service = Executors.newFixedThreadPool(2, new ThreadFactory {
        override def newThread(r: Runnable): Thread = {
            lock.lock()
            try {
                val thread = new Thread(r)
                if(colors.isEmpty) {
                    colors.put(thread.getId, "red")
                } else {
                    colors.put(thread.getId, "green")
                }
                thread
            } finally {
                lock.unlock()
            }
        }
    })
    implicit val ec = ExecutionContext.fromExecutorService(service)

    val MAX_LEVEL = 30
    implicit val duration: Timeout = 20 seconds
    val RECURSION: String = "__recursion__"
    type StringMap = mutable.Map[String, String]
    var idGenerator = 0;

    def getNextId = {
        idGenerator += 1;
        idGenerator;
    }

    def evaluate(ast: Expression) : Expression = {
        var input = ast;
        val gamma = alphaConversion(input, new Gamma())
        log.debug("After alpha conversion:{}", input.toString)
        var result = betaReduce(input, gamma)
        var count = 1;
        for (count <- 1 to MAX_LEVEL if result._1 != input && !result._2.isExceeded) {
            input = result._1;
            result = betaReduce(input, result._2)
        }
        if (result._2.isExceeded) {
            throw new EvaluatorException("Max recursion depth exceeded. Possibly diverging " +
                "expression")
        }
        rename(result._1, gamma.reverse())
    }

    def execute(program: ListBuffer[(String, Expression)]): Expression = {
        val gamma = new Gamma()
        for ((variable, expression) <- program) {
            gamma.addFuture(variable, Future {
                var exp = expression
                val newGamma = alphaConversion(exp, gamma)
                var result = betaReduce(exp, newGamma)
                var count = 1;
                for (count <- 1 to MAX_LEVEL if result._1 != exp && !result._2.isExceeded) {
                    exp = result._1;
                    result = betaReduce(exp, result._2)
                }
                if (result._2.isExceeded) {
                    throw new EvaluatorException("Max recursion depth exceeded. Possibly diverging " +
                        "expression")
                }
                rename(result._1, newGamma.reverse())
                finalizeEvaluation(result, gamma, variable)
            })
        }
        implicit val timeout = Timeout(60 seconds)
        Await.result(gamma.getFuture(program(program.length - 1)._1), timeout.duration)
    }

    def alphaConversion(ast: Expression, gamma: Gamma) : Gamma = {
        ast match {
            case v @ Var(x)  => {
                log.debug("Handling variable")
                if (gamma.contains(x) && Set(x) != gamma(x)) {
                    val alphaRenaming = gamma(x).head
                    v.x = alphaRenaming
                }
                gamma
            }
            case Apply(x: Expression, y:Expression) => {
                log.debug("Handling application")
                val leftGamma = alphaConversion(x, gamma)
                val rightGamma = alphaConversion(y, gamma)
                new Gamma((leftGamma ++ rightGamma))
            }
            case StringConstant(_) => gamma
            case NumericConstant(_) => gamma
            case Cons(head, tail) => alphaConversion(tail, alphaConversion(head, gamma))
            case l @ Lambda(Var(x), body:Expression) => {
                if (!gamma.contains(x)) {
                    log.debug("Adding bound variable {} to gamma", x)
                    gamma += (x -> x)
                } else {
                    val renamedVar = "var" + getNextId
                    gamma += (x -> renamedVar)
                    l.x.x = renamedVar
                }
                alphaConversion(body, gamma)
            }
        }
    }

    def substitute(body: Expression, substitution : (Expression, Expression)) : Expression = {
        body match {
            case v @ Var(_) => {
                if (v == substitution._1) {
                    substitution._2
                } else {
                    v
                }
            }
            case s @ StringConstant(_) => s
            case n @ NumericConstant(_) => n
            case Lambda(x, body) => {
                Lambda(substitute(x, substitution).asInstanceOf[Var],
                    substitute(body, substitution))
            }
            case Cons(head, tail) => Cons(substitute(head, substitution),
                substitute(tail, substitution))
            case Apply(x, y) => {
                Apply(substitute(x, substitution), substitute(y, substitution))
            }
        }
    }

    def betaReduce(ast: Expression, gamma: Gamma): (Expression, Gamma)= {
        if (gamma.isExceeded) {
            (ast, gamma)
        } else {
            gamma.inc()
            val result : (Expression, Gamma) = ast match {
                case v@Var(_) => {
                    log.debug("Nothing further to reduce in {}", v.toString)
                    (v, gamma)
                }
                case s @ StringConstant(_) => {
                    log.debug("Nothing further to reduce in {}", s.toString)
                    (s, gamma)
                }
                case n @ NumericConstant(_) => {
                    log.debug("Nothing further to reduce in {}", n.toString)
                    (n, gamma)
                }
                case Cons(head, tail) => {
                    val reducedHead = betaReduce(head, gamma)
                    val reducedTail = betaReduce(tail, reducedHead._2)
                    (Cons(reducedHead._1, reducedTail._1), reducedTail._2)
                }
                case l@Lambda(Var(_), body: Expression) => {
                    val reducedBody = betaReduce(body, gamma)
                    (Lambda(l.x, reducedBody._1), reducedBody._2)
                }
                case Apply(x, y) => {
                    val reduceY = betaReduce(y, gamma)
                    x match {
                        case Var(_) => (Apply(x, reduceY._1), reduceY._2)
                        case l @ Lambda(arg, body) => (substitute(body, (arg, reduceY._1)), reduceY._2);
                        case Apply(a, b) => {
                            val reduceX = betaReduce(x, gamma)
                            (Apply(reduceX._1, reduceY._1), new Gamma((reduceX._2 ++ reduceY._2)))
                        }
                    }
                }
            }
            gamma.dec()
            result
        }
    }

    private def recurseIfProceeding(expression: Expression, gamma: Gamma, substituted: Expression) = {
        if (substituted == expression) {
            (substituted, gamma)
        } else {
            betaReduce(substituted, gamma)
        }
    }

    def rename(expression: Expression, gamma: StringMap): Expression = {
        expression match {
            case v @ Var(_) => {
                if (gamma.contains(v.x)) {
                    v.x = gamma(v.x)
                }
                v
            }
            case s @ StringConstant(_) => s
            case n @ NumericConstant(_) => n
            case Cons(x, y) => Cons(rename(x, gamma), rename(y, gamma))
            case Lambda(x, e) => {
                Lambda(rename(x, gamma).asInstanceOf[Var], rename(e, gamma))
            }
            case Apply(x, y) => {
                Apply(rename(x, gamma), rename(y, gamma))
            }
        }
    }

    def depth(le: Expression) : Int = {
        le match {
            case Var(_) => 1
            case StringConstant(_) => 1
            case NumericConstant(_) => 1
            case Lambda(Var(_), body: Expression) => 1 + depth(body)
            case Cons(x, y) => depth(x) + depth(y)
            case Apply(x, y) => 1 + Math.max(depth(x), depth(y))
        }
    }

    def finalizeEvaluation(pair: (Expression, Gamma), gamma: Gamma, variable: String): Expression
    = {
        pair._1 match {
            case Apply(Apply(Apply(Var("connect"), hostEntry), portEntry), argsEntry) => {
                val host = hostEntry match {
                    case StringConstant(value) => value
                    case Var(hostVar) => {
                        val currentThread = Thread.currentThread()
                        val hostVarFuture = gamma.getFuture(hostVar)
                        implicit val timeout = Timeout(20 seconds)
                        if (!hostVarFuture.isCompleted) {
                            println(color(timestamp() + " Id:" + currentThread.getId + " EVAL-" +
                                variable +" Blocked on " + hostVar))
                        } else {
                            println(color(timestamp() + " Id:" + currentThread.getId + " EVAL-" + variable +
                                " Using " + hostVar))
                        }

                        val expression = Await.result(hostVarFuture, timeout.duration)
                        if (expression.isInstanceOf[StringConstant]) {
                            expression.asInstanceOf[StringConstant].value
                        } else {
                            throw new EvaluatorException("Connect function requires string " +
                                "value as the host")
                        }
                    }
                    case x => throw new EvaluatorException("Unsupported entry type for host:" +
                        x.getClass.getName)
                }
                val port = portEntry match {
                    case NumericConstant(value) => value
                    case Var(portVar) => {
                        val portVarFuture = gamma.getFuture(portVar)
                        implicit val timeout = Timeout(20 seconds)
                        val currentThread = Thread.currentThread()
                        if (!portVarFuture.isCompleted) {
                            println(color(timestamp() + " Id:" + currentThread.getId + " EVAL-" +
                                variable +" Blocked on " + portVar))
                        } else {
                            println(color(timestamp() + " Id:" + currentThread.getId + " " + "EVAL-" + variable +
                                " Using " + portVar))
                        }
                        val expression = Await.result(portVarFuture, timeout.duration)
                        if (expression.isInstanceOf[NumericConstant]) {
                            expression.asInstanceOf[NumericConstant].value
                        } else {
                            throw new EvaluatorException("Connect function requires numeric " +
                                "value as the port")
                        }
                    }
                }
                val args = argsEntry match {
                    case Var(argsVar) => {
                        val currentThread = Thread.currentThread()
                        val argsVarFuture = gamma.getFuture(argsVar)
                        if (!argsVarFuture.isCompleted) {
                            println(color(timestamp() + " Id:" + currentThread.getId + " EVAL-" +
                                variable +" Blocked on " + argsVar))
                        } else {
                            println(color(timestamp() + " Id:" + currentThread.getId + " " + "EVAL-" + variable +
                                " Using " + argsVar))
                        }
                        implicit val timeout = Timeout(20 seconds)
                        Await.result(argsVarFuture, timeout.duration).toString
                    }
                    case x => x.toString
                }

                val currentThread = Thread.currentThread()
                println(color(timestamp() + " Id:" + currentThread.getId + " EVAL-" +
                    variable + " Connecting to " + host + ":" + port +
                    " with arguments:" + args ))
                Thread.sleep(3000)
                val response = random.nextString(10)
                println(color(timestamp() + " Id:" + currentThread.getId + " EVAL-" + variable +
                    " Server response:" + response))
                StringConstant(response)
            }
            case Apply(Var("parseResponse"), argsEntry) => {
                val args = argsEntry match {
                    case Var(argsVar) => {
                        val argsVarFuture = gamma.getFuture(argsVar)
                        implicit val timeout = Timeout(20 seconds)
                        val currentThread = Thread.currentThread()
                        println(color(currentThread.getName + " " + currentThread.getId + " Waiting " +
                            "for dependent variable " + argsVar + " to be computed"))
                        Await.result(argsVarFuture, timeout.duration).toString
                    }
                    case x => x.toString
                }
                println(color("Parsing response with arguments:" + args))
                val parsedResponse = random.nextString(10)
                println(color("Parsed response:" + parsedResponse))
                StringConstant(parsedResponse)
            }
            case x => x
        }
    }

    def timestamp() : String = {
        val now = Calendar.getInstance()
        now.get(Calendar.HOUR_OF_DAY) + ":" + now.get(Calendar.MINUTE) + ":" + now.get(Calendar.SECOND)
    }
    def color(msg: String) : String = {
        colors.get(Thread.currentThread().getId) match {
            case "red" => red(msg)
            case "green" => green(msg)
            case null => magenta(msg)
        }

    }
    def red(msg : String) : String = ansi"%red{$msg}"
    def green(msg : String) : String = ansi"%green{$msg}"
    def magenta(msg : String) : String = ansi"%magenta{$msg}"
}


