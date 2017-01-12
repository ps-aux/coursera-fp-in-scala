def init[T](xs: List[T]): List[T] = xs match {
  case Nil => throw new Error("Init of empty list")
  case List(x) => Nil
  case y :: ys => y :: init(ys)
}

def removeAt[T](xs: List[T], n: Int) = xs.take(n) ::: xs.drop(n + 1)


def flatten(list: List[Any]): List[Any] = list match {
  case Nil => Nil
  case x :: xs => x match {
    case l: List[Any] => flatten(l) ::: flatten(xs)
    case a: Any => a :: flatten(xs)
  }
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, ys1) => ys
    case (xs1, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if (x > y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }


def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x * x)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => xs.span(y => y == x) match {
    case (l1, l2) => l1 :: pack(l2)
  }
}


pack(List("a", "a", "a", "b", "c", "c", "a"))

List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

def encode[T](list: List[T]): List[(T, Int)] =
  pack(list) map (l => (l.head, l.length))


encode(List("a", "a", "a", "b", "c", "c", "a"))

List(("a", 3), ("b", 1), ("c", 2), ("a", 1))




def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]()) (f(_) :: _)

mapFun[Int, String](List(1, 2, 3), x => (x + 3 ).toString)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0) ((_,x) => x + 1 )

lengthFun(List(2,3,4))















