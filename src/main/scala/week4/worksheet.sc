import week3.{Empty, IntSet, NonEmpty}

val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))

/*val b: Array[IntSet] = a
b(0) = a
val s: NonEmpty = a(0)*/


trait Expr {

  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Var(n) => n
    case Prod(e1, e2) => showInProd(e1) + " * " + showInProd(e2)
  }

  def showInProd(e: Expr): String = e match {
    case Sum(_, _) => "(" + e.show + ")"
    case _ => e.show
  }

}

case class Number(val n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Var(name: String) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr {

}

val s = Sum(Number(1), Prod(Number(2), Number(3)))
val s2 = Prod(Number(1), Sum(Number(2), Number(3)))

s.show
s2.show

Sum(Prod(Number(2), Var("x")), Var("y")).show
Prod(Sum(Number(2), Var("x")), Var("y")).show


val l = List(4, 8, 3, 99, 11, 0)

def isort(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

isort(l)

def foo(x: Int)(y: Int) = x * y

//val x = foo(5)

def f = foo(3)(4)
