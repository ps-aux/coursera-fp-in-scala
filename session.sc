import scala.annotation.tailrec

object session {

  def fac(n: Int) = {

    @tailrec
    def loop(n: Int, acc: Int): Int = {
      if (n == 1)
        acc
      else
        loop(n - 1, acc * n)
    }

    loop(n, 1)
  }


  fac(5)

}