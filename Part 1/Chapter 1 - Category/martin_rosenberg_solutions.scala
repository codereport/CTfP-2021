object martin_rosenberg_solutions {

  // 1.4.1. Implement, as `best as you can`, the identity function in your
  // favorite language (or the second favorite, if your favorite language
  // happens to be Haskell).

  /** @see [[Predef.identity]] */
  def identity[T](x: T): T = x

  // 1.4.2. Implement the composition function in your favorite language. It
  // takes two functions as arguments and returns a function that is their
  // composition.

  /** @see [[Function1.compose]], [[Function1.andThen]] */
  def compose[A, B, C](g: B => C, f: A => B): A => C = x => g(f(x))

  /** You can "extend" existing classes by implicitly converting to and from
    * another class. This "adds" the compose operator `∘` to Function1 (without
    * sacrificing `A => B` syntactic sugar).
    */
  implicit class Function1WithComposeOperator[-T1, +R](f: T1 => R) {
    //noinspection NonAsciiCharacters
    def ∘[A](g: A => T1): A => R = f.compose(g)
  }

  // 1.4.3. Write a program that tries to test that your composition function
  // respects identity.

  def testCompose(): Unit = {
    def sayHi(x: Any): String = s"Hi, ${x}!"

    val sayHiFirst = compose(identity[String], sayHi)
    val sayHiLast = compose(sayHi, identity[Any])

    val ref = new {}
    assert(sayHiFirst(ref) == sayHi(ref), "Compose does not respect identity first.")
    assert(sayHiLast(ref) == sayHi(ref), "Compose does not respect identity last.")
  }

  // 1.4.4. Is the world-wide web a category in any sense? Are links morphisms?

  /* The web does *resemble* a category, but I don't think it is one. For one
   * thing, identity is not always available. Either refreshing the page or the
   * page linking to itself could count - but it's possible for refreshing the
   * page to display something different, e.g. a form being cleared, or a login
   * timing out; and not all pages have self-links.
   */

  // 1.4.5. Is Facebook a category, with people as objects and friendships as
  // morphisms?

  /* No. If Person A is friends with Person B, and Person B is friends with
   * Person C, but Persons A and C are not friends, then friendship is not a
   * morphism.
   */

  // 1.4.6. When is a directed graph a category?

  /* All nodes in the graph would need arrows to themselves, at least
   * implicitly (like the implicit hydrogens in organic chemistry molecules).
   * The arrows also must compose, so there likewise would need to be arrows
   * representing the compositions - again, implicitly should do. I could
   * imagine a case in which the arrows are defined such that they do not align
   * with morphisms, but I cannot think of a specific example.
   */

}
