object Solution {

  /**
    * 1. Show that the two functions phi and psi that form the Yoneda isomorphism in Haskell are inverses of each other.
    *
    * phi :: ((a -> x) -> F x) -> F a
    * phi alpha = alpha id
    *
    * psi :: F a -> ((a -> x) -> F x)
    * psi fa h = fmap h fa
    */
  // phi . psi = id = psi . phi
  //
  // LHS:
  // phi . psi = id
  //
  // phi . psi :: (F a -> ((a -> x) -> F x) -> F a)
  //
  // given, fa:: Fa
  //
  // phi . (psi (fa))                    # applying fa
  // phi . (\h => fmap h fa)
  // phi (\h => fmap h fa)               # alpha as lambda
  // (\h => fmap h fa) (id)              # definition of phi
  // fmap id fa
  // fa                                  # fmap identity law
  //
  // applying fa to phi . psi gives fa back
  //
  // RHS
  // psi . phi = id
  //
  // psi . phi :: (((a -> x) -> F x) -> F a -> ((a -> x) -> F x)))
  //
  // given, alpha:: (((a -> x) -> F x), for some x
  // given h:: a -> x
  // let fa:: Fa = alpha(id)
  // let fx:: Fx = alpha(h)
  // fx = fmap h fa
  //
  // psi . (phi (alpha))                  # applying alpha
  // psi . (alpha (id))
  // psi . (fa)
  // (\h => fmap h fa)
  // (\h => fx)
  // alpha
  //
  // applying alpha to psi . phi gives alpha back
  //

  /**
    * 2. A discrete category is one that has objects but no morphisms other than identity morphisms. How does the Yoneda
    * lemma work for functors from such a category?
    */
  // Yoneda lemma: Natural transformations from hom functor (a) to F are in 1:1 correspondence with elements of Fa
  //
  // C: Category with only identity morphisms
  // functor: C(a, _) for some object a in C, maps all objects to a single set, the empty set, except a which is mapped
  // to singleton set of (identity function).
  //
  // Empty set is the initial object in the category of sets. Unique function from empty set to every other set, absurd.
  //
  // hom functor is between C and Set,
  // for another functor F between C and Set
  //
  // The component of Natural transformation from hom functor to any other Functor F is absurd function.
  //
  // absurd: from falsity follows anything, function from Void (empty set) to Fa.
  //
  // every Natural transformation between C(a, _) and F, corresponds to an element of Fa, which is a set.
  //

  /**
    * 3. A list of units [()] contains no other information but its length. So, as a data type, it can be considered an
    * encoding of integers. An empty list encodes zero, a singleton [()] (a value, not a type) encodes one, and so on.
    * Construct another representation of this data type using the Yoneda lemma for the list functor.
    */
  val zero: List[Unit] = Nil
  val one: List[Unit] = List(())
  val two: List[Unit] = List((), ())
  val three: List[Unit] = List((), (), ())
  val four: List[Unit] = List((), (), (), ())

  // Natural Transformation between F and G, F ~> G
  trait ~>[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  trait Functor[F[_]] {
    def fmap[A, B](f: A => B)(fa: F[A]): F[B]
  }

  type Reader[A] = Unit => A

  val readerFunctor: Functor[Reader] = new Functor[Reader] {
    override def fmap[A, B](f: A => B)(fa: Reader[A]): Reader[B] = f compose fa
  }

  val listFunctor: Functor[List] = new Functor[List] {
    override def fmap[A, B](f: A => B)(fa: List[A]): List[B] = fa.map(f)
  }

  val readerToListTransform0: Reader ~> List = new (Reader ~> List) {
    override def apply[A](f: Reader[A]): List[A] = listFunctor.fmap(f)(zero)
  }

  val readerToListTransform1: Reader ~> List = new (Reader ~> List) {
    override def apply[A](f: Reader[A]): List[A] = listFunctor.fmap(f)(one)
  }

  val readerToListTransform2: Reader ~> List = new (Reader ~> List) {
    override def apply[A](f: Reader[A]): List[A] = listFunctor.fmap(f)(two)
  }

  val readerToListTransform3: Reader ~> List = new (Reader ~> List) {
    override def apply[A](f: Reader[A]): List[A] = listFunctor.fmap(f)(three)
  }

  val readerToListTransform4: Reader ~> List = new (Reader ~> List) {
    override def apply[A](f: Reader[A]): List[A] = listFunctor.fmap(f)(four)
  }

  // Yoneda lemma: Natural transformations from hom functor to F are in 1:1 correspondence with elements of Fa
  // here we have hom functor with a = Unit, C(Unit, _)
  //
  // Elements of F a = List[Unit] are zero, one, two, three, four ...
  // As a datatype, Reader ~> List (Reader to List Natural transformation), can be considered as an encoding of integers
  //

}
