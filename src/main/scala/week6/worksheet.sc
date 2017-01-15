val l = List(1, 2, 3)
l.map(_ * 3)
l flatMap (c => List("e", "e"))


def isPrime(n: Int): Boolean =
  !((2 until n) map (n % _) exists (_ == 0))

def isPrime2(n: Int): Boolean =
  (2 until n) forall (n % _ != 0)

isPrime(17)
isPrime(31)

isPrime2(17)
isPrime2(31)

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for {
    x <- xs
    y <- ys
  } yield x * y).sum


def isSafe(col: Int, queens: List[Int]): Boolean = {

  def isSafe(queensZip: List[(Int, Int)]): Boolean = {
    val qCol = queensZip.head._1
    val index = queensZip.head._2
    if (Set(qCol, qCol + index, qCol - index) contains col) false
    else isSafe(queensZip.tail)
  }

  isSafe(queens zip (1 to queens.length))

}


class Poly(terms0: Map[Int, Double]) {

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

  def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

  def plus(other: Poly) =
    new Poly((other.terms foldLeft terms) (addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
    terms updated(term._1, terms(term._1) + term._2)


  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

p1 + p2
p1 plus p2


val m1 = Map(4 -> 4, 5 -> 5)
val m2 = Map(4 -> 3, 7 -> 9)

m1 ++ m2
m1 + (3 -> 3) + (4 -> 999)
