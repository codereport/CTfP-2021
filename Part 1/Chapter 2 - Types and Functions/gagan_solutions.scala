
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

/*
Pure functions
 */
object TypesAndFunctions {

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = synchronized(getOrElseUpdate(key, f(key)))
  }

  @tailrec
  def factorial(n: Int, acc: Int = 1): Int =
    if (n == 0) acc else factorial(n - 1, acc * n)

  def main(args: Array[String]): Unit = {

    // Part 1. Try to memoize a function from your standard library that you normally use to produce random numbers.
    // Does it work?
    val rng = new Random()
    val memoized = memoize(rng.nextInt)

    println(memoized(6))
    println(rng.nextInt())

    println(memoized(6))
    println(rng.nextInt())

    println(memoized(6))
    println(rng.nextInt())

    println(memoized(6))
    println(rng.nextInt())
    // never produce the same output so NO.

    // Part 2. Most random number generators can be initialized with a seed. Implement a function that takes a seed, calls the
    // random number generator with that seed, and returns the result. Memoize that function. Does it work?
    val memoizedWithSeed: Int => Int => Int = Memoization.memoize((seed: Int) =>
      (n: Int) => new Random(seed).nextInt(n))

    val rand: Int => Int = memoizedWithSeed(6)

    println(rand(5))
    println(new Random(6).nextInt(5))

    println(rand(5))
    println(new Random(6).nextInt(5))

    println(rand(5))
    println(new Random(6).nextInt(5))
    // always produce the same output so YES.
  }

  // all functions from Bool to Bool

  def b1(b: Boolean): Boolean = true
  def b2(b: Boolean): Boolean = false
  def b3(b: Boolean): Boolean = b
  def b4(b: Boolean): Boolean = !b

  // picture of a category whose only objects are the types Void, Unit and Bool

  // objects: Nothing (), Unit (()), Boolean (true, false)

  def unit[A](a: A): Unit = ()

  def nothing[A](a: A): Nothing = nothing()

  // FROM Unit
  def True(): Boolean = true
  def False(): Boolean = false

  def unitToVoid(): Nothing = unitToVoid()

  // FROM Void, absurd functions can never be called
  def voidToBool(x: Nothing): Boolean = true
  def voidToUnit(x: Nothing): Unit = ()

  // FROM Bool
  def boolToUnit(b: Boolean): Unit = ()
  def boolToVoid(b: Boolean): Nothing = boolToVoid(b) // or throw an exception

}
