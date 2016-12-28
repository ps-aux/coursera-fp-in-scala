import week3.Empty
import week3.NonEmpty


val t = new NonEmpty(4, Empty, Empty)

val t2 = t.incl(5)

val t3 = new NonEmpty(3, Empty, Empty)

t2 union t3