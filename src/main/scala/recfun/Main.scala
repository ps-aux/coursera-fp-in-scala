package recfun

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == 1 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true

    def isBalanced(rest: List[Char], opened: Int): Boolean = {
      if (rest.isEmpty)
        return opened == 0

      val h = rest.head
      if (h == '(')
        isBalanced(rest.tail, opened + 1)
      else if (h == ')')
        if (opened == 0)
          return false
        else
          isBalanced(rest.tail, opened - 1)
      else
        isBalanced(rest.tail, opened)
    }

    isBalanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0)
      1
    else if (money < 0)
      0
    else if (coins.isEmpty)
      0
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
