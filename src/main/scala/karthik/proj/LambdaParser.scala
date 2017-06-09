package karthik.proj

/**
  * Created by ksubramanian on 5/15/17.
  */
import com.typesafe.scalalogging.Logger

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharSequenceReader

class LambdaParser extends DebugStandardTokenParsers with PackratParsers {
    type Tokens = StdLexical
    val log = Logger("LambdaEvaluator")
    val lexical = new CustomLambdaLexer
    private val eol = sys.props("line.separator")
    lexical.delimiters ++= Seq("\\", ".", "λ", "(", ")", "=", ";", "++")
//    lexical.reserved ++= Seq("connect", "parse_response")

    lazy val defs                                           = rep(defn)
    lazy val defn                                          = ident ~ "=" ~ lambdaExpression ~
        ";" ^^ {
        case id ~ "=" ~ t ~ ";" => (id, t)
    }
    lazy val lambdaExpression: PackratParser[Expression] = applicationOptions | otherExpressionTypes
    lazy val otherExpressionTypes                        = value | cons | parens | lambda
    lazy val lambda: PackratParser[Lambda]               = ("λ" | "\\") ~> variable ~ "." ~ parens ^^ {
        case arg ~ "." ~ body => Lambda(arg, body)
    }
    lazy val parens: PackratParser[Expression]             = "(" ~> lambdaExpression <~ ")" | lambdaExpression
    lazy val applicationOptions: PackratParser[Apply] = "(" ~> application <~ ")" | application
    lazy val application: PackratParser[Apply] = lambdaExpression ~ otherExpressionTypes ^^ {
            case left ~ right => Apply(left, right)
        }

    lazy val cons: PackratParser[Cons] = "(" ~> "++" ~> value ~ (cons | value) <~ ")" ^^ {
        case head ~ tail => Cons(head, tail)
    }
    lazy val value: PackratParser[Expression] = variable | stringConstant | numericConstant
    lazy val variable: PackratParser[Var] = ident ^^ {x => Var(x.toString)}
    lazy val stringConstant: PackratParser[StringConstant] = stringLit ^^ {x => StringConstant(x)}
    lazy val numericConstant: PackratParser[NumericConstant] = numericLit ^^ {x => NumericConstant(x.toInt)}
}

class CustomLambdaLexer extends StdLexical {
    override def letter = elem("letter", c => c.isLetter && c != 'λ')

}

object SimpleLambdaParser extends LambdaParser {

    def parse(input: CharSequence) = {
        val parseResult = parsePhrase(new CharSequenceReader(input))
        val result = parseResult match {
            case Success(t, _) => t
            case NoSuccess(msg, next) => {
                throw new ParserException("Error parsing line:" + next.pos.line + " at column:" +
                    next.pos.column + " for entry:" + next.pos.longString + " Cause:" + msg)
            }
        }
        collection.mutable.ListBuffer(result: _*)
    }

    def parsePhrase(input: CharSequenceReader) = {
        val tokens = new lexical.Scanner(input)
        phrase(defs)(tokens)
    }
}

trait DebugStandardTokenParsers extends StdTokenParsers {
    class Wrap[+T](name:String,parser:Parser[T]) extends Parser[T] {
        def apply(in: Input): ParseResult[T] = {
            val first = in.first
            val pos = in.pos
            val offset = in.offset
            val t = parser.apply(in)
            println(name+".apply for token "+first+
                " at position "+pos+" offset "+offset+" returns "+t)
            t
        }
    }

    implicit def toWrapped(name:String) = new {
        def !!![T](p:Parser[T]) = new Wrap(name,p) //for debugging
        //def !!![T](p:Parser[T]) = p              //for production
    }
}