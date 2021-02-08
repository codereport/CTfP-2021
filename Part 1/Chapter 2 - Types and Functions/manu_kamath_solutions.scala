import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

object Main extends App {
    /** 
     * Q1: memoize
     */
    def memoize[A, B] (f: A => B) : A => B = {

        val cache = mutable.Map.empty[A,B]

        x => cache.getOrElseUpdate(x, f(x))
    }

    def testMemoization() = {
        def expensive_add(a: Int, b: Int): Int = {
            Thread.sleep(7000)
            a + b
        }

        // Not production worthy for benchmarking but will do
        // http://biercoff.com/easily-measuring-code-execution-time-in-scala/
        def time[R](block: => R): R = {
            val t0 = System.nanoTime()
            val result = block    // call-by-name
            val t1 = System.nanoTime()
            println("Elapsed time: " + (t1 - t0) + "ns")
            result
        }
        
        val memo = memoize(expensive_add _ tupled)
        println("Before memoizing:")
        var v1 = time { memo(6,8) }
        println("After memoizing:")
        var v2 = time { memo(6,8) }

        // ensure new value is not the memoized one
        println("Different argument")
        var v3 = time {memo(7,9) }
    }

    // testMemoization()

    /** 
     * Q2: Memoizing rand function
     */
    // What's really being asked is if rand is a pure function.
    // This shouldn't work since RNG stream is stateful.

    // dangit, since nextInt is a member function of Random, we need
    // to modify the memoize to take in a Unit function
    def memoize_unit[T] (f: () => T) : () => T = {
        
        val cache = mutable.Map.empty[Unit,T]
        
        () => cache.getOrElseUpdate((), f())
    }

    def testRandom() = {
        val random = new Random()
        val memoed_rng = memoize_unit(random.nextInt)
        val rand1 = memoed_rng()
        val rand2 = memoed_rng()
        assert(rand1 == rand2)
        assert(rand1 != Random.nextInt()) // statistically improbable that this fails
    }

    // testRandom()

    /** 
     * Q3: Pseudo-RNG
     */

    // here actually we take a seed for Random.next_*, so the original memoize works
    def pseudoRNG(seed : Int) : Int = {
        val random = new Random()
        val nextRandVal = memoize(random.nextInt)
        nextRandVal(seed)
    }

    def test_randseed() = {
        println("Rand Numbers With seed 10: " pseudoRNG(10), pseudoRNG(10)) // memoization doesn't give us the same value
    }

    test_randseed()

     /** 
     * Q4: Determine which functions are pure
     */

    /**
     * 1) Y
     * 2) N
     * 3) N (has side effect w/ print) -> ACTUALLY THIS ISN'T QUITE RIGHT, it is not pure but it's
     * because the memoized function will return true after the first call so the print is only executed the first time.
     * 4) Y -> THIS IS WRONG, the static keyword causes y to have state. Tricky tricky
    */

    /** 
     * Q5: How many different functions from Bool to Bool? Can you implement them all?
     */

    /**
      * 2^2 = 4, yes basically two that either return the same or negated value as that passed in,
      * and the other true return either true or false irrespective of the passed argument
      */
      def identical(x : Boolean) : Boolean = x
      def negated(x : Boolean) : Boolean = !x
      def _true(x: Boolean): Boolean = true
      def _false(x: Boolean): Boolean = false
    
    /** 
     * Q6: Category consisting of Void, Unit and Bool
     */

    //  See manu_kamath_diagram_Q6.md
}