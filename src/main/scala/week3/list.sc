trait List[+T] {

  def isEmpty: Boolean

  def head: T

  def tail: List[T]

}

object Nil extends List[Nothing] {

  override def isEmpty: Boolean = true


  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: List[Nothing] = throw new NoSuchElementException("Nil.tail")

  override def toString = "-"

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

  override def isEmpty: Boolean = false

  override def toString = head + "," + tail.toString
}


def nth[T](l: List[T], index: Int): T =
  if (index == 0) l.head
  else nth(l.tail, index - 1)

val l = new Cons(3, new Cons(2, Nil))

l
