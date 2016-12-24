
object App {

  def loop(): Int = loop

  def foo(x: Int, y: => Int) = x

  def main(args: Array[String]): Unit = {
    println(foo(4, loop))
  }

}
