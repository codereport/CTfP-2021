import challenges._24_F_Algebras.Solution.RingF.{RAdd, RMul, RNeg, ROne, RZero}

object Solution {

  trait Functor[F[_]] {
    def map[A, B](f: A => B): F[A] => F[B]
  }

  object Functor {
    def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

    implicit val listFunctor: Functor[List] = new Functor[List] {
      override def map[A, B](f: A => B): List[A] => List[B] = _.map(f)
    }
  }

  type Algebra[F[_], A] = F[A] => A

  type CoAlgebra[F[_], A] = A => F[A]

  case class Fix[F[_]](value: F[Fix[F]])

  def catamorphism[F[_], A](F: Functor[F]): Algebra[F, A] => Fix[F] => A =
    algebra =>
      fixF => (algebra compose F.map(catamorphism(F)(algebra)))(fixF.value)

  def anamorphism[F[_], A](F: Functor[F]): CoAlgebra[F, A] => A => Fix[F] =
    coAlgebra =>
      a => Fix((F.map(anamorphism(F)(coAlgebra)) compose coAlgebra)(a))

  sealed trait RingF[A]

  object RingF {
    case class RZero[A]() extends RingF[A] // can make it case object, +A, extends Nothing
    case class ROne[A]() extends RingF[A]
    case class RAdd[A](x: A, y: A) extends RingF[A]
    case class RMul[A](x: A, y: A) extends RingF[A]
    case class RNeg[A](a: A) extends RingF[A]

    def evalZ: Algebra[RingF, Int] = {
      case RZero()    => 0
      case ROne()     => 1
      case RAdd(x, y) => x + y
      case RMul(x, y) => x * y
      case RNeg(a)    => -a
    }

    implicit val ringFunctor: Functor[RingF] = new Functor[RingF] {
      override def map[A, B](f: A => B): RingF[A] => RingF[B] = {
        case RZero()    => RZero()
        case ROne()     => ROne()
        case RAdd(x, y) => RAdd(f(x), f(y))
        case RMul(x, y) => RMul(f(x), f(y))
        case RNeg(a)    => RNeg(f(a))
      }
    }
  }

  /**
    * 1. Implement the evaluation function for a ring of polynomials of one variable. You can represent a polynomial as a
    * list of coefficients in front of powers of 𝑥. For instance, 4𝑥2 − 1 would be represented as (starting with the zero’th power)[-1, 0, 4].
    */
  type Polynomial[X] = (X, List[Int])

  def evalPolynomial: Algebra[Polynomial, Int] = p => {
    val x = p._1
    p._2
      .foldLeft((0, 1)) {
        case ((acc, xn), coefficient) => (acc + coefficient * xn, x * xn)
      }
      ._1
  }

  /**
    * 2. Generalize the previous construction to polynomials of many independent variables, like 𝑥2𝑦 − 3𝑦3𝑧.
    */
  type TypeNPolynomial[X] = List[Polynomial[X]]

  def evalTypeNPolynomial: Algebra[TypeNPolynomial, Int] =
    p => p.foldLeft(0)(_ + evalPolynomial(_))

  /**
    * 3. Implement the algebra for the ring of 2 × 2 matrices.
    */
  type Matrix = (Int, Int, Int, Int)

  def evalMatrix: Algebra[RingF, Matrix] = {
    case RZero() =>
      (0, 0, 0, 0)
    case ROne() =>
      (1, 1, 1, 1)
    case RAdd((a, b, c, d), (e, f, g, h)) =>
      (a + e, b + f, c + g, d + h)
    case RMul((a, b, c, d), (e, f, g, h)) =>
      (a * e + b * g, a * f + b * h, c * e + d * g, c * f + d * h)
    case RNeg((a, b, c, d)) =>
      (-a, -b, -c, -d)
  }

  /**
    * 4. Define a co-algebra whose anamorphism produces a list of squares of natural numbers.
    */
  sealed trait ListF[A]
  case class NilF[A]() extends ListF[A]
  case class ConsF[A](e: Int, a: A) extends ListF[A]

  object ListF {
    val algebra: Algebra[ListF, List[Int]] = {
      case NilF()      => Nil
      case ConsF(e, a) => e :: a
    }

    implicit val functor: Functor[ListF] = new Functor[ListF] {
      override def map[A, B](f: A => B): ListF[A] => ListF[B] = {
        case NilF()      => NilF()
        case ConsF(e, a) => ConsF(e, f(a))
      }
    }
  }

  val CoAlgebraSquares: CoAlgebra[ListF, List[Int]] = {
    case Nil    => NilF()
    case h :: t => ConsF(h * h, t)
  }

  val squares: List[Int] => List[Int] =
    catamorphism(ListF.functor)(ListF.algebra) compose
      anamorphism(ListF.functor)(CoAlgebraSquares)

  /**
    * 5. Use unfoldr to generate a list of the first 𝑛 primes.
    */
  def unFoldR[S](f: S => Option[(Int, S)])(s: S): List[Int] = {

    type MaybeF[X] = Option[(Int, X)]

    implicit val functor: Functor[MaybeF] = new Functor[MaybeF] {
      override def map[A, B](f: A => B): MaybeF[A] => MaybeF[B] = {
        case None         => None
        case Some((a, s)) => Some((a, f(s)))
      }
    }

    val algebra: Algebra[MaybeF, List[Int]] = {
      case Some((a, as)) => a :: as
      case None          => Nil
    }

    (catamorphism(functor)(algebra) compose anamorphism(functor)(f))(s)
  }

  def unFoldRManual[S](f: S => Option[(Int, S)])(s: S): List[Int] =
    f(s) match {
      case Some((a, s)) => a :: unFoldRManual(f)(s)
      case None         => Nil
    }

  def firstNPrimes(n: Int): List[Int] = {
    unFoldR[(Int, Stream[Int])] {
      case (`n`, _) => None
      case (found, stream) => Some((stream.head, (found + 1, stream.tail.filter(_ % stream.head != 0))))
    }((0, Stream.from(2)))
  }

  def main(args: Array[String]): Unit = {
    // Q4
    println(squares(List(1, 2, 3)))

    // Q5
    println(unFoldR[Int](s => if (s < 10) Some(s, s + 2) else None)(1))
    println(firstNPrimes(5))
  }

}
