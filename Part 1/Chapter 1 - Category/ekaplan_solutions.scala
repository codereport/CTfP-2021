object ekaplan_solutions {
  /**
   * 1.4.1
   * Implement, as best as you can, the identity function in
   * your favorite language (or the second favorite, if your
   * favorite language happens to be Haskell).
   */
  def id[T](x: T): T = x

  /**
   * 1.4.2
   * Implement the composition function in your favorite language. It
   * takes two functions as arguments and returns a function that is
   * their composition
   */
  def compose[A, B, C](f: A => B, g: B => C): A => C =
    (x: A) => g(f(x)) // or just `f andThen g`

  /**
   * 1.4.3
   * Write a program that tries to test that your composition function
   * respects identity.
   */
  def testIdentityComposition(): Unit = {
    import Numeric.Implicits._

    // Straw man function from Numeric types to String type
    def doubleString[T: Numeric](x: T): String = (x + x).toString

    val doubleStringLeftId = compose(id[Int], doubleString[Int])
    val doubleStringRightId = compose(doubleString[Int], id[String])

    assert(doubleString[Int](1) == doubleStringLeftId(1), "Left identity does not hold")
    assert(doubleString[Int](1) == doubleStringRightId(1), "Right identity does not hold")
  }

  /**
   * 1.4.4
   * Is the world-wide web a category in any sense? Are links morphisms?
   *
   * If links were morphisms, the web would really only be a category if
   * every page had a link to itself (representing the identity morphism)
   * and if a link existed between every page and every other page that
   * was transitively linked to it, which, ostensibly, is not the case.
   */

  /**
   * 1.4.5
   * Is Facebook a category, with people as objects and friendships as
   * morphisms?
   *
   * Probably not, for the same reasons as the previous question.
   */

  /**
   * 1.4.6
   * When is a directed graph a category?
   *
   * 1. All nodes must have an edge incident to itself
   * 2. For all nodes A, B, and C for which there exists a directed
   * edge from A to B and from B to C, there must exist a directed
   * edge from A to C.
   */
}
