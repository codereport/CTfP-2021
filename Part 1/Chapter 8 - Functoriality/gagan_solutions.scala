object Solution {

  /**
    * 1. Show that the data type is a bifunctor.
    *   data Pair a b = Pair a b
    * For additional credit implement all three methods of Bifunctor and use equational reasoning to show
    * that these definitions are compatible with the default implementations whenever they can be applied.
    */
  trait Bifunctor[F[_, _]] {
    def bimap[A, B, C, D](f: A => C)(g: B => D): F[A, B] => F[C, D] =
      first(f) compose second(g)

    def first[A, B, C](f: A => C): F[A, B] => F[C, B] =
      bimap(f)(identity[B])

    def second[A, B, D](g: B => D): F[A, B] => F[A, D] = bimap(identity[A])(g)
  }

  case class Pair[A, B](a: A, b: B)

  object Pair {
    implicit val bifunctor: Bifunctor[Pair] = new Bifunctor[Pair] {
      override def bimap[A, B, C, D](f: A => C)(g: B => D): Pair[A, B] => Pair[C, D] = {
        case Pair(a, b) => Pair(f(a), g(b))
      }

      override def first[A, B, C](f: A => C): Pair[A, B] => Pair[C, B] = {
        case Pair(a, b) => Pair(f(a), b)
      }

      override def second[A, B, D](g: B => D): Pair[A, B] => Pair[A, D] = {
        case Pair(a, b) => Pair(a, g(b))
      }
    }
  }

  // identity law
  // bimap(id)(id) Pair(a, b)
  // Pair(id(a), id(b))
  // Pair(a, b)
  //
  // composition laws
  // bimap(f'.f)(g'.g) Pair(a, b)
  // Pair(f'.f(a), g'.g(b))
  // Pair(f'(f(a)), g'(g(b)))
  // bimap(f')(g') Pair(f(a), g(b))
  // bimap(f')(g') bimap(f)(g) Pair(a, b)

  // bimap equivalence to default
  // bimap(f)(g) Pair(a, b)
  // Pair(f(a), g(b))
  // bimap(id)(g) Pair(f(a), b)
  // second(g) Pair(f(a), b)
  // bimap(f)(id) second(g) Pair(a, b)
  // first(f) second(g) Pair(a, b)
  //
  // first equivalence to bimap
  // first(f) Pair(a, b)
  // Pair(f(a), b)
  // Pair(f(a), id(b))
  // bimap(f)(id) Pair(a, b)
  //
  // second equivalence to bimap
  // second(g) Pair(a, b)
  // Pair(a, g(b))
  // Pair(id(a), g(b))
  // bimap(id)(g) Pair(a, b)

  /**
    * 2. Show the isomorphism between the standard definition of Maybe and this de-sugaring:
    * type Maybe a = Either (Const () a) (Identity a)
    * Hint: Define two mappings between the two implementations. For additional credit, show that they are the inverse
    * of each other using equational reasoning.
    */
  case class Const[C, A](c: C)
  type Id[A] = A

  type Maybe[A] = Either[Const[Unit, A], Id[A]]

  def f[A](m: Maybe[A]): Option[A] = {
    m match {
      case Left(_)      => None
      case Right(value) => Some(value)
    }
  }

  def g[A](o: Option[A]): Maybe[A] = {
    o match {
      case Some(value) => Right(value)
      case None        => Left(Const(()))
    }
  }

  // they are inverse of each other
  // f compose g = id
  //
  // f compose g None
  // f(g(None)
  // f(Left(Const(())))
  // None
  //
  // f compose g Some(5)
  // f(g(Some(5))
  // f(Right(5))
  // Some(5)
  //
  //
  // g compose f = id
  //
  // g compose f Left(Const(()))
  // g(f(Left(Const(()))))
  // g(None)
  // Left(Const(()))
  //
  // g compose f Right(5)
  // g(f(Right(5)))
  // g(Some(5))
  // Right(5)

  /**
    * 3. Let’s try another data structure. I call it a PreList because it’s a precursor to a List.
    * It replaces recursion with a type parameter b.
    * data PreList a b = Nil | Cons a b
    * You could recover our earlier definition of a List by recursively applying PreList to itself (we’ll see how it’s done
    * when we talk about fixed points).
    * Show that PreList is an instance of Bifunctor.
    */
  type PreList[A, B] = Either[Nil.type, Pair[A, B]]

  type FunctorialPreList[A, B] = Either[Const[Unit, A], (Id[A], Id[B])]

  // It is constructed as a sum of (a functorial primitives and (product of (two functorial primitives)))
  // We know product of (two functorial primitives) is a bifunctor

  trait Functor[F[_]] {
    def fmap[A, B](f: A => B)(fa: F[A]): F[B]
  }

  case class BiComp[BF[_, _], FU[_], GU[_], A, B](v: BF[FU[A], GU[B]])

  implicit def bicompBiFunctor[BF[_, _], FU[_], GU[_]](
      implicit BF: Bifunctor[BF],
      FU: Functor[FU],
      GU: Functor[GU]) = {
    // partially-applied type BiComp
    type BiCompAB[A, B] = BiComp[BF, FU, GU, A, B]
    new Bifunctor[BiCompAB] {
      override def bimap[A, B, C, D](f1: A => C)(
          f2: B => D): BiCompAB[A, B] => BiCompAB[C, D] = {
        case BiComp(x) => BiComp(BF.bimap(FU.fmap(f1))(GU.fmap(f2))(x))
      }
    }
  }

  /**
    * 4. Show that the following data types define bifunctors in a and b:
    * data K2 c a b = K2 c
    * data Fst a b = Fst a
    * data Snd a b = Snd b
    * For additional credit, check your solutions against Conor McBride’s paper
    * 1 Clowns to the Left of me, Jokers to the Right .
    */
  type K2[C, A, B] = C
  type Fst[A, B] = A
  type Snd[A, B] = B

  // We show that they are universal constructions from functorial primitives Id and Const

  type FunctorialK2[C, A, B] = Either[Const[C, A], Const[C, B]]
  type FunctorialFst[A, B] = Either[Id[A], Const[Unit, B]]
  type FunctorialSnd[A, B] = Either[Const[Unit, A], Id[B]]

  /**
    * 5. Define a bifunctor in a language other than Haskell. Implement bimap for a generic pair in that language.
    */
  // refer Scala Bifunctor for Q1

  /**
    * 6. Should std::map be considered a bifunctor or a profunctor in the two template arguments Key and T? How would you
    * redesign this data type to make it so?
    */
  // It is equivalent to function1 from Key to Maybe T. And function1 is a profunctor

  trait Profunctor[F[_, _]] {
    def bimap[A, B, C, D]: (A => B) => (C => D) => F[B, C] => F[A, D] =
      f => g => lmap(f) compose rmap[B, C, D](g)
    def lmap[A, B, C]: (A => B) => F[B, C] => F[A, C] =
      f => bimap(f)(identity[C])
    def rmap[A, B, C]: (B => C) => F[A, B] => F[A, C] =
      bimap[A, A, B, C](identity[A])
  }

  implicit val function1Profunctor: Profunctor[Function] = new Profunctor[Function1] {
    override def bimap[A, B, C, D]
      : (A => B) => (C => D) => (B => C) => (A => D) =
      ab => cd => bc => cd compose bc compose ab
    override def lmap[A, B, C]: (A => B) => (B => C) => (A => C) =
      f => g => g compose f
    override def rmap[A, B, C]: (B => C) => (A => B) => (A => C) =
      f => g => f compose g
  }

}
