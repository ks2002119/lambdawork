package karthik.proj

/**
  * Created by ksubramanian on 5/4/17.
  */
import org.scalatest._
import karthik.proj.SimpleLambdaParser._

import scala.util.parsing.input.CharSequenceReader

class LanguageSpec extends FlatSpec with Matchers {
    private val eol = sys.props("line.separator")
    def addEnd(s: String) = s + ";"

    def compare(expression: String, result: Expression, varName : String = "x") = {
        val lambdaExpression = SimpleLambdaParser.parse(addEnd(expression))
        assert(lambdaExpression(0)._1 == varName);
        assert(lambdaExpression(0)._2 == result);
    }

    "Lambda Parser" should "parse simple string constant" in {
        compare("x = \"test\"", StringConstant("test"))
    }

    it should "parse simple numeric constant" in {
        compare("x = 1", NumericConstant(1))
    }

    it should "parse simple apply expression" in {
        compare("x = a b", Apply(Var("a"), Var("b")))
    }

    it should "parse apply expression with parenthesis" in {
        compare("x = (a b)", Apply(Var("a"), Var("b")))
    }

    it should "parse lambda expression" in {
        compare("x = λa.b", Lambda(Var("a"), Var("b")))
    }

    it should "parse lambda expression with parenthesis" in {
        compare("x = (λa.b)", Lambda(Var("a"), Var("b")))
    }

    it should "parse alternate form of lambda expression" in {
        compare("x = \\a.b", Lambda(Var("a"), Var("b")))
    }

    it should "parse alternate form of lambda expression with parenthesis" in {
        compare("x = (\\a.b)", Lambda(Var("a"), Var("b")))
    }

    it should "parse cons expression" in {
        compare("g=(++ x (++ \"test\" (++ \"abc\" y)))",
            Cons(Var("x"), Cons(StringConstant("test"), Cons(StringConstant("abc"), Var("y")))), "g")
    }

    it should "parse cons expression directly" in {
        object ConsParser extends LambdaParser {

            def parse(s: CharSequence): Expression = {
                val tokens = new lexical.Scanner(new CharSequenceReader(s))
                phrase(cons)(tokens) match {
                    case Success(t, _) => t
                    case NoSuccess(msg, next) => throw new IllegalArgumentException(
                        "Could not parse '" + s + "' near '" + next.pos.longString + ": " + msg)
                }
            }
        }
        val lambdaExpression = ConsParser.parse("(++ x (++ \"test\" (++ \"abc\" y)))")
        assert(lambdaExpression ==
            Cons(Var("x"), Cons(StringConstant("test"), Cons(StringConstant("abc"), Var("y")))));
    }

    it should "parse connect as variable" in {
        compare("x=connect", Var("connect"))
    }

    it should "parse parseResponse as variable" in {
        compare("x=parseResponse", Var("parseResponse"))
    }
}
