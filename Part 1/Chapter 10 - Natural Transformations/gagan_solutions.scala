object Solution {

  trait ~>[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  trait Functor[F[_]] {
    def fmap[A, B](f: A => B)(fa: F[A]): F[B]
  }

  val listFunctor: Functor[List] = new Functor[List] {
    override def fmap[A, B](f: A => B)(fa: List[A]): List[B] = fa match {
      case Nil          => Nil
      case head :: tail => f(head) :: fmap(f)(tail)
    }
  }

  val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](f: A => B)(fa: Option[A]): Option[B] = fa match {
      case Some(value) => Some(f(value))
      case None        => None
    }
  }

  /**
    * 1. Define a natural transformation from the Maybe functor to the list functor.
    * Prove the naturality condition for it.
    */
  val optionToListTransform: Option ~> List = new ~>[Option, List] {
    override def apply[A](f: Option[A]): List[A] = f match {
      case Some(value) => List(value)
      case None        => Nil
    }
  }

  // alphaA: F[A] => G[A]
  // alphaB: F[B] => G[B]
  // f: A => B
  // Naturality condition: G f . alphaA == alphaB . F f
  // F = optionFunctor, G = listFunctor
  // for a: A
  //
  // case Some
  //
  // LHS
  // (G f . alphaA) (Some(a))
  // (G f) (alphaA (Some(a)))
  // (G f) (List(a)) // naturality definition
  // f(a) :: Nil
  //
  // RHS
  // (alphaB . F f)(Some(a))
  // (alphaB) (F f (Some(a)))
  // alphaB (Some(f(a))) // naturality definition
  // f(a) :: Nil
  //
  // case None
  //
  // LHS
  // (G f . alphaA) (None)
  // (G f) (alphaA (None))
  // (G f) (Nil) // naturality definition
  // Nil
  //
  // RHS
  // (alphaB . F f)(None)
  // (alphaB) (F f (None))
  // alphaB (None) // naturality definition
  // Nil

  /**
    * 2. Define at least two different natural transformations between Reader () and the list functor.
    * How many different lists of () are there?
    */
  type Reader[A] = Unit => A

  val readerFunctor: Functor[Reader] = new Functor[Reader] {
    override def fmap[A, B](f: A => B)(fa: Reader[A]): Reader[B] = f compose fa
  }

  val readerToListTransform0: Reader ~> List = new ~>[Reader, List] {
    override def apply[A](f: Reader[A]): List[A] = Nil
  }

  val readerToListTransform1: Reader ~> List = new ~>[Reader, List] {
    override def apply[A](f: Reader[A]): List[A] = List(f(()))
  }

  val readerToListTransform2: Reader ~> List = new ~>[Reader, List] {
    override def apply[A](f: Reader[A]): List[A] = List(f(()), f(()))
  }

  val readerToListTransform3: Reader ~> List = new ~>[Reader, List] {
    override def apply[A](f: Reader[A]): List[A] = List(f(()), f(()), f(()))
  }

  // Yoneda lemma: members of this family of natural transformations are in 1:1 correspondence with elements of List[A]

  /**
    * 3. Continue the previous exercise with Reader Bool and Maybe.
    */
  type ReaderBool[A] = Boolean => A

  val readerBoolFunctor: Functor[ReaderBool] = new Functor[ReaderBool] {
    override def fmap[A, B](f: A => B)(fa: ReaderBool[A]): ReaderBool[B] =
      f compose fa
  }

  val readerBoolToOptionTransform0: ReaderBool ~> Option =
    new ~>[ReaderBool, Option] {
      override def apply[A](f: ReaderBool[A]): Option[A] = None
    }

  val readerBoolToOptionTransform1: ReaderBool ~> Option =
    new ~>[ReaderBool, Option] {
      override def apply[A](f: ReaderBool[A]): Option[A] = Some(f(true))
    }

  val readerBoolToOptionTransform2: ReaderBool ~> Option =
    new ~>[ReaderBool, Option] {
      override def apply[A](f: ReaderBool[A]): Option[A] = Some(f(false))
    }

  /**
    * 4. Show that horizontal composition of natural transformation satisfies the naturality condition (hint: use components).
    * It’s a good exercise in diagram chasing.
    */
  // We have the functors F, G and the natural transformations:
  //
  // αa: F a -> F'a
  // βa: G a -> G'a
  //
  // Commuting squares
  // (G'αa . βFa) = (βF'b . Gαb) // horizontal composition
  //
  // F'f . αa = αb . Ff
  // GF'f . Gαa = Gαb . GFf // lift G
  //
  // G'F'f . βF'a = βF'b . GF'f
  //
  // Naturality condition for Horizontal composition
  // (G' . F') f . (β ◦ α)a = (β ◦ α)b . (G . F) f
  //
  // (β ◦ α)b . (G . F) f =
  // (βF'b . Gαb) . G . F f =
  // βF'b . G  . F' f . Gαa =
  // G'F'f . βF'a . Gαa =
  // (G' . F') f . (β ◦ α)a =

  /**
    * 5. Write a short essay about how you may enjoy writing down the evident diagrams needed to prove the interchange law.
    */
  //
  // α: F -> F'
  // α': F' -> F''
  //
  // β: G -> G'
  // β': G' -> G''
  //
  // (β' . β) ◦ (α' . α)     = (β' ◦ α') . (β ◦ α)
  // (G -> G'') ◦ (F -> F'') = (G'F' -> G''F'') . (GF -> G'F')
  // (GF -> G''F'')          = (GF -> G''F'')
  //

  /**
    * 6. Create a few test cases for the opposite naturality condition of transformations between different Op functors.
    * Here’s one choice:
    * and
    * op :: Op Bool Int
    * op = Op (\x -> x > 0)
    *
    * f :: String -> Int
    * f x = read x
    */
  type Op[R, A] = A => R

  type OpBool[A] = Op[Boolean, A]

  trait ContravariantFunctor[F[_]] {
    def contramap[A, B](f: B => A)(fa: F[A]): F[B]
  }

  // f: B => A
  // contramapG f . alphaA = alphaB . contramapF f
  //

  type OpString[A] = Op[String, A]

  def contravariantFunctor[R]
    : ContravariantFunctor[({ type OpR[A] = Op[R, A] })#OpR] = {
    new ContravariantFunctor[({ type OpR[A] = Op[R, A] })#OpR] {
      override def contramap[A, B](f: B => A)(fa: A => R): B => R = fa compose f
    }
  }

  val opBoolContraFunctor = contravariantFunctor[Boolean] // F

  val opStringContraFunctor = contravariantFunctor[String] // G

  def alphaA[A](opBool: OpBool[A]): OpString[A] =
    a => if (opBool(a)) "True" else "False"

  // B: Long, A: Double
  val f: Long => Double = _.toDouble

  val Ff: OpBool[Double] => OpBool[Long] = opBoolContraFunctor.contramap(f)
  val Gf: OpString[Double] => OpString[Long] = opStringContraFunctor.contramap(f)

  // Naturality Condition
  val lhs: (OpBool[Double]) => OpString[Long] = Gf compose alphaA[Double]
  val rhs: (OpBool[Double]) => OpString[Long] = alphaA[Long] _ compose Ff

  def main(args: Array[String]): Unit = {
    val opBoolDouble: OpBool[Double] = _ == 2.0

    val x: OpString[Long] = lhs(opBoolDouble)
    val y: OpString[Long] = rhs(opBoolDouble)

    assert(x(2L) == y(2L))
    assert(x(5L) != y(2L))
  }
}
