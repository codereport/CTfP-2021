package com.martinbrosenberg.exercises.ctfp.part1

import scala.collection.{concurrent, mutable}
import scala.util.{Random, Try}

object martin_rosenberg_solutions {

  // 2.7.1. Define a higher-order function (or a function object) `memoize` in
  // your favorite language. This function takes a pure function `f` as an
  // argument and returns a function that behaves almost exactly like `f`,
  // except that it only calls the original function once for every argument,
  // stores the result internally, and subsequently returns this stored result
  // every time it's called with the same argument. You can tell the memoized
  // function from the original by watching its performance. For instance, try
  // to memoize a function that takes a long time to evaluate. You’ll have to
  // wait for the result the first time you call it, but on subsequent calls,
  // with the same argument, you should get the result immediately.

  /** @todo Make this work for recursion
    * @todo Make this polymorphic?
    */
  def memoize[I, O](f: I => O): I => O = {
    val cache = concurrent.TrieMap.empty[I, O]
    key => cache.getOrElseUpdate(key, f(key))
  }

  /** An alternate approach.
    *
    * @note Inheriting HashMap is deprecated as of Scala 2.13, and will not be
    *       possible in the future.
    * @see [[mutable.HashMap]]
    */
  def memoize2[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = synchronized(getOrElseUpdate(key, f(key)))
  }

  // 2.7.2. Try to memoize a function from your standard library that you
  // normally use to produce random numbers. Does it work?

  /* It works in the sense that the result is memoized, but it no longer works
   * as a random number generator. It will generate one random number for each
   * bound 0..n, and cache it.
   */

  /** @todo Make this work for nullary `nextInt` */
  val nextIntMemoized: Int => Int = memoize(new Random().nextInt)

  def testNextIntMemoized(): Try[Unit] = Try {
    assert(nextIntMemoized(Int.MaxValue) != nextIntMemoized(Int.MaxValue))
  }

  // 2.7.3. Most random number generators can be initialized with a seed.
  // Implement a function that takes a seed, calls the random number generator
  // with that seed, and returns the result. Memoize that function. Does it
  // work?

  /* It works exactly like it did without specifying a seed - because the only
   * difference is that a seed is being specified manually instead of
   * automatically by the system.
   */

  def seededNextInt(seed: Long)(n: Int): Int = new Random(seed).nextInt(n)

  val seededNextIntMemoized: Long => Int => Int = memoize(seededNextInt)

  def testSeededNextIntMemoized(): Try[Unit] = Try {
    val testRng = seededNextIntMemoized(1234567890987654321L)
    assert(testRng(Int.MaxValue) != testRng(Int.MaxValue))
  }

  // 2.7.4. Which of these functions are pure? Try to memoize them and observe
  // what happens when you call them multiple times: memoized and not.

  object Challenge_2_7_4 {

    // 2.7.4.a. The factorial function from the example in the text:
    def fact(n: Int): Int = {
      var result = 1
      for (i <- 2 to n) {
        result *= i
      }
      result
    }

    /* Pure. It produces the same output for each input with no side effects. */

    val factMemoized: Int => Int = memoize(fact)

    def testFactMemoized(): Try[Unit] = Try {
      assert(factMemoized(10) == factMemoized(10))
    }

    // 2.7.4.b.
    io.StdIn.readChar

    /* Not pure. This produces different outputs with the same input `()` each
     * time. Additionally, because of it not programmatically accepting an
     * input, the builtin can't even be memoized. However, wrapped in this
     * thunk, it can be memoized - and it simply stops accepting new characters
     * from stdin after the first time, because it already knows that the
     * character will be [whichever one you entered first].
     */

    def readCharThunk(x: Unit): Char = io.StdIn.readChar()
    val readCharMemoized: Unit => Char = memoize(readCharThunk)

    def testReadCharMemoized(): Try[Unit] = Try {
      assert(readCharMemoized(()) != readCharMemoized(()))
    }

    // 2.7.4.c.
    def f(): Boolean = {
      println("Hello!")
      true
    }

    /* Not pure. The point of this is the side-effect of printing "Hello!"; if
     * you memoize it, the effect only happens the first time.
     *
     * Need to capture stdout to test this.
     */

    def fThunk(x: Unit): Boolean = f()
    val fMemoized: Unit => Boolean = memoize(fThunk)

    // 2.7.4.d.
    val g: Int => Int = {
      var y: Int = 0
      (x: Int) => {
        y += x
        y
      }
    }

    /* Not pure. Side effect of changing `y`, AND `x` maps onto a different
     * return value every time. Memoizing results in `y` being incremented
     * exactly once, the first time, by the first `x` value, but not
     * changing thereafter - so the memoization mostly works, and the function
     * mostly doesn't.
     */

    val gMemoized: Int => Int = memoize(g)
    def testGMemoized(): Try[Unit] = Try {
      assert(gMemoized(3) == gMemoized(3))
    }
  }

  // 2.7.5. How many different functions are there from Bool to Bool? Can you
  // implement them all?

  object Challenge_2_7_5 {
    def booleanId(x: Boolean): Boolean = x
    def booleanNegate(x: Boolean): Boolean = !x
    def booleanTrue(x: Boolean): Boolean = true
    def booleanFalse(x: Boolean): Boolean = false
  }

  // 2.7.6. Draw a picture of a category whose only objects are the types
  // `Void`, `()` (unit), and `Bool`; with arrows corresponding to all possible
  // functions between these types. Label the arrows with the names of the
  // functions.

  /** The graph includes all of the following, plus all compositions of them. */
  object Challenge_2_7_6 {

    def unitId(x: Unit): Unit = ()
    def unitTrue(x: Unit): Boolean = true
    def unitFalse(x: Unit): Boolean = false
    def unitVoid(x: Unit): Nothing = ??? // bottom

    def absurdUnit(x: Nothing): Unit = ()
    def absurdTrue(x: Nothing): Boolean = true
    def absurdFalse(x: Nothing): Boolean = false
    def absurdId(x: Nothing): Nothing = ??? // bottom

    def booleanUnit(x: Boolean): Unit = ()
    def booleanId(x: Boolean): Boolean = x
    def booleanNegate(x: Boolean): Boolean = !x
    def booleanTrue(x: Boolean): Boolean = true
    def booleanFalse(x: Boolean): Boolean = false
    def booleanVoid(x: Boolean): Nothing = ??? // bottom
    
  }

}
