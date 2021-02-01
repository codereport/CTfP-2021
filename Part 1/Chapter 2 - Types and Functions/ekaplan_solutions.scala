import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.Random

object ekaplan_solutions {

  /**
   * 2.7.1
   * Define a higher-order function (or a function object) memoize in
   * your favorite language. This function takes a pure function f as
   * an argument and returns a function that behaves almost exactly
   * like f, except that it only calls the original function once for every
   * argument, stores the result internally, and subsequently returns
   * this stored result every time it’s called with the same argument.
   * You can tell the memoized function from the original by watching its performance.
   * For instance, try to memoize a function that
   * takes a long time to evaluate. You’ll have to wait for the result
   * the first time you call it, but on subsequent calls, with
   */
  final class Memoize0[T, M <: mutable.Map[Unit, T]](memo: M, f: () => T) extends (() => T) {
    def apply(): T = synchronized(memo.getOrElseUpdate((), f()))
  }

  object Memoize0 {
    def apply[T](f: () => T) = new Memoize0(mutable.HashMap[Unit, T](), f)
  }

  final class Memoize1[A, B, M <: mutable.Map[A, B]](memo: M, f: A => B) extends (A => B) {
    def apply(a: A): B = synchronized(memo.getOrElseUpdate(a, f(a)))
  }

  object Memoize1 {
    def apply[A, B](f: A => B) = new Memoize1(mutable.HashMap[A, B](), f)
  }

  def testMemoization(): Unit = {
    /**
     * Timer method, which given a code block to call by-name,
     * will invoke that block, returning a tuple of its result
     * and the duration of execution
     */
    def time[T](f: => T): (T, Duration) = {
      val start = System.nanoTime()
      (f, Duration.fromNanos(System.nanoTime() - start))
    }

    /**
     * Expensive function to test effects of memoization
     */
    def isPrime(num: Int): Boolean = (2 to num - 1) forall (num % _ != 0)

    val isPrimeMemoized = Memoize1(isPrime)

    val x = 1000000000
    val (_, firstTime) = time(isPrimeMemoized(x))
    val (_, secondTime) = time(isPrimeMemoized(x))
    val ratio = firstTime / secondTime

    println(s"Memoization results in ${ratio}x performance improvement")
  }

  /**
   * 2.7.2
   * Try to memoize a function from your standard library that you
   * normally use to produce random numbers. Does it work?
   */
  def memoizeRandom(): Unit = {
    val random = new Random()
    val nextIntMemoized = Memoize0(random.nextInt)

    // This assertion will fail, since memoization will
    // result in the same "random" value as was generated
    // by the first invocation of the function.
    assert(nextIntMemoized() != nextIntMemoized())
  }

  /**
   * 2.7.3
   * Most random number generators can be initialized with a seed.
   * Implement a function that takes a seed, calls the random number
   * generator with that seed, and returns the result. Memoize that
   * function. Does it work?
   */
  def memoizeRandomSeed(): Unit = {
    val random = new Random()
    val nextIntMemoized = Memoize1(random.nextInt)

    // This assertion will still fail, since our memoized
    // results will map the seed to whatever value was first
    // generated for that seed
    assert(nextIntMemoized(100) != nextIntMemoized(100))

    // For calls with different seeds, we will get different results
    // (but subsequent calls with those seeds as arguments still will
    // not produce new random values). So this assertion should be
    // satisfied, but there's still a behavior change, here.
    assert(nextIntMemoized(100) != nextIntMemoized(101))
  }

  /**
   * 2.7.4
   * Which of these C++ functions are pure? Try to memoize them
   * and observe what happens when you call them multiple times:
   * memoized and not.
   * (a) The factorial function from the example in the text.
   * (b) std::getchar()
   * (c) bool f() {
   * std::cout << "Hello!" << std::endl;
   * return true;
   * }
   * (d) int f(int x) {
   * static int y = 0
   * y += x;
   * return y;
   * }
   *
   * (a) Pure
   * (b) Not pure
   * (c) Not pure
   * (c) Not pure
   */

  /**
   * 2.7.5
   * How many different functions are there from Bool to Bool? Can
   * you implement them all?
   *
   * 4
   */
  def boolId(x: Boolean): Boolean = x

  def boolNegate(x: Boolean): Boolean = !x

  def boolTrue(x: Boolean): Boolean = true

  def boolFalse(x: Boolean): Boolean = false

  /**
   * 2.7.6
   * Draw a picture of a category whose only objects are the types
   * Void, () (unit), and Bool; with arrows corresponding to all possible functions between these types. Label the arrows with the
   * names of the functions.
   *
   * voidId :: Void -> Void
   * unitId :: Unit -> Unit
   * boolId :: Bool -> Bool
   * unitAbsurd :: Void -> Unit
   * boolAbsurd :: Void -> Bool
   * true :: Unit -> Bool
   * false :: Unit -> Bool
   * not :: Bool -> Bool
   * boolTrue :: Bool -> Bool
   * boolFalse :: Bool -> Bool
   * boolUnit :: Bool -> Unit
   */
}
