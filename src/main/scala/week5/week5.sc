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