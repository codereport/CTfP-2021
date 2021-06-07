object Solution {

  /**
    * 1. Express the co-Yoneda embedding in Haskell/Scala.
    */
  // Natural Transformation between functors F and G, F ~> G
  trait ~>[F[_], G[_]] {
    def apply[X](f: F[X]): G[X]
  }

  // Natural Transformations between (A => _) and F[_]
  type Yoneda[F[_], A] = ({type Reader[X] = A => X})#Reader ~> F

  // Yoneda lemma: Isomorphism between Yoneda[F[_], A] and F[A], given F is a Functor

  trait Functor[F[_]] {
    def map[A, B](f: A => B)(fa: F[A]): F[B]
  }

  def fromYoneda[F[_], A](y: Yoneda[F, A]): F[A] =
    y.apply[A](identity[A])

  def toYoneda[F[_]: Functor, A](fa: F[A]): Yoneda[F, A] = new Yoneda[F, A] {
     def apply[X](f: A => X): F[X] = implicitly[Functor[F]].map(f)(fa)
  }

  // Natural Transformations between (_ => A) and F[_]
  type CoYoneda[F[_], A] = ({type OP[X] = X => A})#OP ~> F

  // CoYoneda lemma: Isomorphism between CoYoneda[F[_], A] and F[A], given F is a Contravariant functor

  trait ContravariantFunctor[F[_]] {
    def map[A, B](f: B => A)(fa: F[A]): F[B]
  }

  def fromCoYoneda[F[_], A](y: CoYoneda[F, A]): F[A] =
    y.apply[A](identity[A])

  def toCoYoneda[F[_]: ContravariantFunctor, A](fa: F[A]): CoYoneda[F, A] = new CoYoneda[F, A] {
    def apply[X](f: X => A): F[X] = implicitly[ContravariantFunctor[F]].map(f)(fa)
  }

  // Yoneda embedding: Natural Transformation between (A => _) and (B => _) is isomorphic to B => A
  type YonedaEmbedding[A, B] = ({type Reader[X] = A => X})#Reader ~> ({type Reader[X] = B => X})#Reader

  def fromYonedaEmbedding[A, B](yE: YonedaEmbedding[A, B]): B => A =
    yE.apply[A](identity[A])

  def toYonedaEmbedding[A, B](bToA: B => A): YonedaEmbedding[A, B] = new YonedaEmbedding[A, B] {
    override def apply[X](ax: A => X): B => X = ax compose bToA
  }

  // CoYoneda embedding: Natural Transformation between (_ => A) and (_ => B) is isomorphic to A => B
  type CoYonedaEmbedding[A, B] = ({type OP[X] = X => A})#OP ~> ({type OP[X] = X => B})#OP

  def fromCoYonedaEmbedding[A, B](yE: CoYonedaEmbedding[A, B]): A => B =
    yE.apply[A](identity[A])

  def toCoYonedaEmbedding[A, B](aToB: A => B): CoYonedaEmbedding[A, B] = new CoYonedaEmbedding[A, B] {
    override def apply[X](xa: X => A): X => B = aToB compose xa
  }

  /**
   * 2. Show that the bijection we established between fromY and btoa is an isomorphism
   * (the two mappings are the inverse of each other).
   */
  // bToA :: b -> a
  //
  // fromY :: (a -> x) -> (b -> x)
  // fromY f b = f (btoA(b))
  //
  // fromY id :: b -> a
  //
  // or in other words...
  //
  // fromYonedaEmbedding :: ((a -> x) -> (b -> x)) -> (b -> a)
  // fromYonedaEmbedding axbx = axbx (id)
  //
  // toYonedaEmbedding :: (b -> a) -> ((a -> x) -> (b -> x))
  // toYonedaEmbedding bToA ax = ax . bToA
  //
  // for Isomorphism-
  // fromYonedaEmbedding . toYonedaEmbedding = id = toYonedaEmbedding . fromYonedaEmbedding
  //
  // LHS
  //
  // fromYonedaEmbedding . toYonedaEmbedding :: (b -> a) -> ((a -> x) -> (b -> x)) -> (b -> a)
  //
  // given bToA:: b -> a
  //
  // (fromYonedaEmbedding . toYonedaEmbedding) (bToA)
  // fromYonedaEmbedding . (toYonedaEmbedding (bToA))
  // fromYonedaEmbedding (\ax => ax . bToA)
  // (\ax => ax . bToA)(id)
  // id . bToA
  // bToA
  //
  // RHS
  //
  // toYonedaEmbedding . fromYonedaEmbedding :: ((a -> x) -> (b -> x)) -> (b -> a) -> ((a -> x) -> (b -> x))
  //
  // given axbx:: ((a -> x) -> (b -> x)), for some x
  //
  // (toYonedaEmbedding . fromYonedaEmbedding) (axbx)
  // toYonedaEmbedding . (fromYonedaEmbedding (axbx))
  // toYonedaEmbedding (axbx (id))
  // (\ax => ax . axbx (id))
  // axbx
  //

  /**
   * 3. Workout the Yoneda embedding for a monoid. What functor corresponds to the monoid’s single object?
   * What natural transformations correspond to monoid morphisms?
   */
  // Yoneda embedding, a -> C(a, _), mapping from object a to hom functor
  //
  // a Monoid is a single object category
  //
  // a -> Set of all functions between a and a
  //
  // then, identity natural transformation corresponds to monoid morphisms
  def idNaturalTransformation[F[_]]: F ~> F = new (F ~> F) {
    override def apply[X](fx: F[X]): F[X] = fx
  }

  /**
   * 4. What is the application of the covariant Yoneda embedding to preorders?
   * (Question suggested by Gershom Bazerman.)
   */
  // Applying Yoneda embedding to a preorder category
  // [C, Set](C(a, _), C(b, _)) ~ C(b, a)
  //
  // hom-set on right is non-empty iff b <= a
  // consequently if b <= a (or right side is inhabited), there's 1:1 correspondence to left side
  // there exists Natural Transformation.
  //
  // functions between (C(a, _), C(b, _)), hom-set C(a, _) and C(b, _) can have at-most one element and
  // singleton set cannot connect to empty set
  //
  // this would mean that if a <= x exists, then b <= also exists,
  // for every x, a <= x then b <= x given b <= a
  //
  // b <= a, iff for all x, a <= x implies b <= x
  //


  /**
   * 5. Yoneda embedding can be used to embed an arbitrary functor category [𝐂, 𝐃] in the functor category [ [𝐂, 𝐃], 𝐒𝐞𝐭].
   * Figure out how it works on morphisms (which in this case are natural transformations).
   */
  // Yoneda embedding maps object a in Category C to hom functor C(a, _) in functor category [C, Set].
  // hom functor C(a, _) is an object in the functor category between C and Set.
  //
  // given the category under consideration is a functor category between some functors C and D
  //
  // then for an object of this category, functor cd1, we'd have a new functor from functor category between [C, D] and Set
  // [C, D](cd1, _), hom functor in functor category between C and D
  //
  // From yoneda lemma: [[C, D], Set]([C, D](cd1, _), F) ~ F cd1
  //
  // choosing functor [C, D](cd2, _) for F, corresponding to functor cd2 part of functor category between C and D
  //
  // [[C, D], Set]([C, D](cd1, _), [C, D](cd2, _)) ~ [C, D](cd2, cd1)
  //
  // The Natural Transformation between hom functors in functor category between C and D are in 1:1 correspondence with
  // Natural transformation between the corresponding functors
  //

}
