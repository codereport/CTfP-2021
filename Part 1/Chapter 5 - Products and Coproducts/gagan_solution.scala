object Solution {

  // 1. Show that the terminal object is unique up to unique isomorphism.
  /*
  Terminal objects t1 and t2
  Both must have identity morphisms, let's call them id1 and id2

  "The terminal object is the object with one and only one morphism coming to it from any object in the category."

  g: t1 => t2 and
  f: t2 => t1

  Then (g compose f) on t2 brings back to t2, but that's the job of id2

  "An isomorphism is an invertible morphism; or a pair of morphisms, one being the inverse of the other."

  This implies that (g compose f), g and f are isomorphic

  The terminal object is unique up to unique isomorphism.
   */

  // 2. What is a product of two objects in a Poset? Hint: Use the universal construction.
  /*
  With product we have mapping in, anybody who claims to have a function to A and B, will have a function to product
  of A and B

             C
            /| \
           / |<=\
          /  ↓   \
       <=/ (A, B) \ <=
        /  /    \  \
       /  /<=  <=\  \
       ↓ ↓       ↓  ↓
        A          B

  functions f and g are factorised

  f = p1 . h
  g = p2 . h

  If Poset had integer objects, then product = lcm(A, B)

  lcm(A, B) <= A
  lcm(A, B) <= B

  Product would be an object wherein it would be ordered less than both A and B, the greatest of all such prospective candidates.
  Since this is a Poset and not total, there's no guarantee there would exist ordering between different product candidates.
  But given the definition of product, every other product candidate would be ordered less than the actual product, `m` exists.
   */

  // 3. What is a coproduct of two objects in a Poset?
  /*
  With Sum we have a unique mapping out, anybody who claims to have a function from A and from B, will have a
  function from sum of A and B

          A           B
         / \         / \
         \  \ <=  <= /  |
          \  ↓     ↓   /
       <=  \ (A + B)  / <=
            \   | <= /
             \  |   /
              \ |  /
              ↓ ↓ ↓
                C

   f = h . l
   g = h . r

   h : A + B => C

  CoProduct would be an object wherein both A and B, would be ordered less than it, the smallest of all such prospective candidates.
  Since this is a Poset and not total, there's no guarantee there would exist ordering between different sum candidates.
  But given the definition of CoProduct, CoProduct would be ordered less than every other CoProduct candidate, `m` exists.
   */

  // 4. Implement the equivalent of Haskell Either as a generic type in your favorite language (other than Haskell).
  sealed trait Either[+A, +B]

  case class Left[+A, +B](value: A) extends Either[A, B]
  case class Right[+A, +B](value: B) extends Either[A, B]

  // 5. Show that Either is a “better” coproduct than int equipped with two injections:

  def i(n: Int): Int = n
  def j(b: Boolean): Int = if (b) 0 else 1

  def m(e: Either[Int, Boolean]): Int = e match {
    case Left(value)  => i(value)
    case Right(value) => j(value)
  }

  // 6. Continuing the previous problem: How would you argue that int with the two injections i and j cannot be “better” than Either?
  // there exists factor m which proves it, and we can generate any m given any i and j

  // 7. Still continuing: What about these injections?

  def ii(n: Int): Int = if (n < 0) n else n + 2
  def jj(b: Boolean): Int = if (b) 0 else 1

  // they both can always be factorized
  // ii = m compose Left(_)
  // jj = m compose Right(_) where m is derived using ii and jj

  // 8. Come up with an inferior candidate for a coproduct of int and bool that cannot be better than Either because it allows multiple acceptable morphisms from it to Either.

  type ProductEither = (Either[Int, Boolean], Either[Int, Boolean])

  // morphisms back
  val f: ProductEither => Either[Int, Boolean] = _._1
  val g: ProductEither => Either[Int, Boolean] = _._2

  // m
  def pair[A](a: A): (A, A) = (a, a)

}
