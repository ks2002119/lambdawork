package karthik.proj

/**
  * Model classes
  */

sealed abstract class Expression

case class Var(var x: String) extends Expression {
    override def toString = x.toString
}

trait Value[T] {
    def getValue(): T
}

case class StringConstant(value: String) extends Expression with Value[String] {
    override def toString = value.toString

    override def getValue() = value
}

case class NumericConstant(value: Int) extends Expression with Value[Int] {

    override def toString = value.toString

    override def getValue() = value
}

case class Cons(head: Expression, tail: Expression) extends Expression {
    override def toString = "(" + head.toString + "," + tail.toString + ")"
}

case class Lambda(x:Var, e:Expression) extends Expression {
    def apply(e: Expression) = Apply(this, x)
    override def toString = "λ" + x + "." + e.toString

}

case class Apply(e1: Expression, e2: Expression) extends Expression {
    override def toString() = "(" + e1.toString + " " + e2.toString + ")"
}