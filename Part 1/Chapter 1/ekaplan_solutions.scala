object ekaplan_solutions extends App {

  // 1.4.1
  def id[T](x: T): T = x

  // 1.4.2
  def compose[A, B, C](f: A => B, g: B => C): A => C =
    (x: A) => g(f(x)) // or just `f andThen g`

  // 1.4.3
  def testComposition(): Unit = {
    import Numeric.Implicits._

    def doubleString[T: Numeric](x: T): String = (x + x).toString

    val doubleIdLeft = compose(id[Int], doubleString[Int])
    val doubleIdRight = compose(doubleString[Int], id[String])

    assert(doubleString[Int](1) == doubleIdLeft(1))
    assert(doubleString[Int](1) == doubleIdRight(1))
  }

  testComposition()
}