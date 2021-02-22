object Solution {
  // 1. Generate a free category from-
  // 1.a A graph with one node and no edges
  // add identity arrow
  // 1.b A graph with one node and one (directed) edge (hint: this edge can be composed with itself)
  // add infinite arrows
  // 1.c A graph with two nodes and a single arrow between them
  // add identity arrows
  def identity[A](a: A): A = a

  // 1.d A graph with a single node and 26 arrows marked with the letters of the alphabet: a, b, c ... z.
  // object = Int
  // morphisms
  val a: Int => Int = ???
  val b: Int => Int = ???
  val c: Int => Int = ???
  val betweenAandB: Int => Int = a andThen b
  val betweenBandC: Int => Int = b andThen c
  val betweenAandC: Int => Int = betweenAandB andThen betweenBandC // free construction
  // ... infinite morphisms

  // 2. What kind of order is this?
  // 2.a A set of sets with the inclusion relation: 𝐴 is included in 𝐵 if every element of 𝐴 is also an element of 𝐵.
  // partial order; subset relationship, if a isSubsetOf b and b isSubsetOf a then a == b
  // 2.b C++ types with the following subtyping relation: T1 is a subtype of T2 if a pointer to T1 can be passed to a function
  // that expects a pointer to T2 without triggering a compilation error.
  // partial order; not total because types don't always have vertical hierarchy

  // 3. Considering that Bool is a set of two values True and False, show that it forms two (set-theoretical) monoids with
  // respect to, respectively, operator && (AND) and || (OR).

  trait Monoid[A] {
    def identity: A

    def combine(x: A, y: A): A
  }

  val boolMonoid1: Monoid[Boolean] = new Monoid[Boolean] {
    override def identity: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  val boolMonoid2: Monoid[Boolean] = new Monoid[Boolean] {
    override def identity: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  // 4. Represent the Bool monoid with the AND operator as a category: List the morphisms and their rules of composition.
  // object Bool, elements true and false
  // morphisms
  def identity(b: Boolean): Boolean = b && true
  def andFalse(b: Boolean): Boolean = b && false

  // 5. Represent addition modulo 3 as a monoid category.
  // monoid category is a single object category
  // object Int type, elements 0, 1, 2
  // morphisms
  // multiple identity morphisms
  def identityMorphisms(n: Int): Int => Int = (a: Int) => (a + 3 * n) % 3 // A

  // other morphisms
  def addOneMorphisms(n: Int): Int => Int = (a: Int) => (a + 1 + 3 * n) % 3 // B
  def addTwoMorphisms(n: Int): Int => Int = (a: Int) => (a + 2 + 3 * n) % 3 // C

  // A . B = B
  // A . C = C
  // B . B = C
  // C . C = B
  // B . C = A

}
