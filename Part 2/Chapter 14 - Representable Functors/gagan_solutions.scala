object Solution {

  /**
    * 1. Show that the hom-functors map identity morphisms in C to corresponding identity functions in 𝐒𝐞𝐭.
    */
  // hom-functor = C(a, _)
  // f :: x -> y, is lifted to C(a, f) :: C(a, x) -> C(a, y) in 𝐒𝐞𝐭
  // C(a, x) = hom-set(a, x) or C(a, x) :: a -> x
  //
  // given h :: a -> x, then C(a, f) h = f . h, function pre-composition
  //
  // for id: x -> x, is lifted to C(a, id) :: C(a, x) -> C(a, x)
  // then C(a, id) h = id . h
  //
  // C(a, id) :: C(a, x) -> C(a, x)
  // C(a, id) h = h, which is identity
  //
  // thus the id morphism in h is lifted to the identity function in 𝐒𝐞𝐭
  //

  /**
    * 2. Show that Maybe is not representable.
    */
  // for functor F[_] to be representable we need
  // alpha :: (a -> x) -> F[x] and
  // beta :: F[x] -> (a -> x) to be invertible, i.e alpha . beta = id  = beta . alpha.
  //
  // In other words F to be isomorphic to Reader functor (a -> _)
  //
  // let a = Int
  // alpha h = map h Some(12)
  // beta Some(x) _ = x
  // beta None _ = ???
  //
  // cannot implement beta for the None case
  // thus,  Maybe/Option functor is not representable

  /**
    * 3. Is the Reader functor representable?
    */
  // yes, Reader functor is isomorphic to itself
  // alpha h = h
  // beta h = h

  /**
    * 4. Using Stream representation, memoize a function that squares its argument.
    */
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Representable[F[_]] {

    def F: Functor[F]

    type Representation

    def index[A](f: F[A]): Representation => A

    def tabulate[A](f: Representation => A): F[A]
  }

  class Stream[A] private (val head: A, val tail: () => Stream[A]) // non empty infinite lazy list

  object Stream {
    def apply[A](head: A, tail: => Stream[A]): Stream[A] = {
      lazy val tl = tail
      new Stream(head, () => tl)
    }
  }

  val StreamIntRepresentable: Representable[Stream] { type Representation = Int } =
    new Representable[Stream] {

      type Representation = Int

      val F: Functor[Stream] = new Functor[Stream] {
        override def map[A, B](fa: Stream[A])(f: A => B): Stream[B] =
          Stream(f(fa.head), map(fa.tail())(f))
      }

      def tabulate[A](f: Int => A): Stream[A] =
        Stream(f(0), tabulate(f compose (_ + 1)))

      def index[A](stream: Stream[A]): Int => A = {
        case 0 => stream.head
        case n => index(stream.tail())(n - 1)
      }
    }

  def main(args: Array[String]): Unit = {
    val memoized: Stream[Int] = StreamIntRepresentable.tabulate(x => x * x)

    val square: Int => Int = StreamIntRepresentable.index(memoized)

    (0 to 10).foreach(x => println(square(x)))
    /*
    0
    1
    4
    9
    16
    25
    36
    49
    64
    81
    100
   */
  }

  /**
    * 5. Show that tabulate and index for Stream are indeed the inverse of each other. (Hint: use induction.)
    */
  // f :: Int => A
  //
  // prove: index(tabulate(f)) = f
  //
  // for case n = 0
  // index(tabulate(f))(0) = f(0)
  // tabulate(f).head
  // f(0)
  //
  // assuming equality holds for n, index(tabulate(f))(n) = f(n)
  //
  // then for n + 1,
  //
  // index(tabulate(f))(n + 1) = f(n + 1)
  // index(tabulate(f).tail)(n + 1 - 1)
  // index(tabulate(f).tail)(n)
  // index(tabulate(f . (_ + 1)))(n)
  // index(tabulate(f . (_ + 2)))(n - 1)
  // ...
  // ...
  // index(tabulate(f . (_ + n)))(1)
  // index(tabulate(f . (_ + n + 1)))(0)
  // tabulate(f . (_ + n + 1)).head
  // f . (_ + n + 1)(0)
  // f((_ + n + 1)(0))
  // f(n + 1)
  //
  // Hence proved using induction

  /**
    * 6. The functor:
    * Pair a = Pair a a
    * is representable. Can you guess the type that represents it? Implement tabulate and index.
    */
  // type Boolean represents it

  type Pair[A] = (A, A)

  val StreamBoolRepresentable: Representable[Pair] { type Representation = Boolean } =
    new Representable[Pair] {
      type Representation = Boolean

      val F: Functor[Pair] = new Functor[Pair] {
        override def map[A, B](fa: (A, A))(f: A => B): (B, B) =
          (f(fa._1), f(fa._2))
      }

      override def tabulate[A](f: Boolean => A): Pair[A] = (f(false), f(true))

      override def index[A](p: Pair[A]): Boolean => A = {
        case false => p._1
        case true  => p._2
      }

    }
}
