package karthik.proj

/**
  * Created by ksubramanian on 5/4/17.
  */
import org.scalatest._
import karthik.proj.SimpleLambdaParser._

class NReduceEngineSpec extends FlatSpec with Matchers {
    private val eol = sys.props("line.separator")
    def addEnd(s: String) = s + ";\n"

    def execAndTestResult(expression: String, result: Expression) = {
        val executionResult = NReduceEngine.execute(parse(addEnd(expression)))
        assert(executionResult == result)
    }

    "NReduceEngine" should "execute simple string constant expression" in {
        execAndTestResult("host = \"test\"", StringConstant("test"))
    }

    it should "execute simple numeric constant expression" in {
        execAndTestResult("port = 1", NumericConstant(1))
    }

    it should "execute simple cons expression" in {
        execAndTestResult("args = (++ foo bar)", Cons(Var("foo"), Var("bar")))
    }

    it should "execute simple apply expression" in {
        execAndTestResult("args = (foo bar)", Apply(Var("foo"), Var("bar")))
    }

    it should "execute simple lambda expression" in {
        execAndTestResult("args=λfoo.bar", Lambda(Var("foo"), Var("bar")))
    }

    it should "execute simple call by value redex" in {
        execAndTestResult("args = ((λfoo.foo)1)", NumericConstant(1))
    }

    it should "execute connect function call" in {
        val executionResult = NReduceEngine.execute(parse("x = (\\a.\\b.\\c.\\d.a b c d) connect " +
    "\"localhost\" 80 \"foo\";"))
        assert(executionResult.isInstanceOf[StringConstant])
    }

    it should "execute multiple statement connect function call" in {
        val expression = addEnd("host = \"localhost\"") + addEnd("port = 80") + addEnd("arg = " +
            "\"foo\"") + addEnd("connectCall = (\\a.\\b.\\c.\\d.a b c d) connect host port arg")
        val executionResult = NReduceEngine.execute(parse(expression))
        assert(executionResult.isInstanceOf[StringConstant])
    }

    it should "execute complex connect function call" in {
        val expression = addEnd("host = \"localhost\"") +
            addEnd("port = 80") +
            addEnd("arg = \"foo\"") +
            addEnd("e = (\\a.\\b.\\c.\\d.a b c d) connect host port arg") +
            addEnd("f = (\\a.\\b.\\c.\\d.a b c d) connect host port e") +
            addEnd("g = (\\a.\\b.\\c.\\d.a b c d) connect host port f") +
            addEnd("h = (\\a.\\b.\\c.\\d.a b c d) connect host port g") +
            addEnd("i = (\\a.\\b.\\c.\\d.a b c d) connect host port h") +
            addEnd("k = (\\a.\\b.\\c.\\d.a b c d) connect host port e") +
            addEnd("l = (\\a.\\b.\\c.\\d.a b c d) connect host port h") +
            addEnd("y = (\\a.\\b.\\c.\\d.a b c d) connect host port j") +
            addEnd("j = (\\a.\\b.\\c.\\d.a b c d) connect y port k") +
            addEnd("m = (\\a.\\b.\\c.\\d.a b c d) connect host port j") +
            addEnd("x = (\\a.\\b.\\c.\\d.a b c d) connect host port m") +
            addEnd("finalArg = (++ x (++ \"test\" (++ \"abc\" y)))") +
            addEnd("finalResult = (\\a.\\b.\\c.\\d.a b c d) connect host port finalArg")
        println(expression)
        val executionResult = NReduceEngine.execute(parse(expression))
        assert(executionResult.isInstanceOf[StringConstant])
    }
}
