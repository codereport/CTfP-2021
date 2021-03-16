object Solution {

  /**
    * Can we turn the Maybe type constructor into a functor by defining:
    * fmap _ _ = Nothing
    * which ignores both of its arguments? (Hint: Check the functor laws.)
    */
  // no, because it violates the identity law on Just x case
  // law: fmap(id) = id
  //
  // fmap id (Just x)
  // Nothing
  // id Nothing

  /**
    * Prove functor laws for the reader functor. Hint: it’s really simple.
    */
  //  instance Functor ((->) r) where
  //      fmap f g = f . g
  //

  // identity law: fmap id  = id
  //
  // fmap id f
  // id . f (by definition)

  // composition law: fmap (g . f) = fmap g . fmap f
  //
  // h: r -> c
  // fmap (g . f) h
  // (by definition)
  // (g . f) . h
  // (composition is associative)
  // g . (f . h)
  // (by definition in reverse)
  // g. (fmap f h)
  // (by definition in reverse)
  // fmap g fmap f h

  /**
    * Implement the reader functor in your second favorite language (the first
    * being Haskell, of course).
    */
  trait Functor[F[_]] {
    def fmap[A, B](f: A => B)(fa: F[A]): F[B]
  }

  implicit def readerFunctor[R]: Functor[({ type f[A] = R => A })#f] = // can be simplified to Functor[R => *]
    new Functor[({ type f[A] = R => A })#f] {
      def fmap[A, B](f: A => B)(g: R => A): (R => B) =
        f compose g
    }

  /**
    * Prove the functor laws for the list functor. Assume that the laws are true
    * for the tail part of the list you’re applying it to (in other words, use induction).
    */
  // data List a = Nil | Cons a (List a)
  // instance Functor List where
  //     fmap _ Nil = Nil
  //     fmap f (Cons x t) = Cons (f x) (fmap f t)
  //

  // identity law: fmap id  = id
  //
  // fmap id Nil
  // (by definition)
  // Nil
  // (definition of id)
  // id Nil
  //
  // fmap id (Cons h t)
  // (by definition)
  // Cons (id h) (fmap id t)
  // (definition of id)
  // Cons h (fmap id t)
  // (by proposition)
  // Cons h t
  // (definition of id)
  // id (Cons h t)

  // composition law: fmap (g . f) = fmap g . fmap f
  //
  // fmap (g . f) Nil
  // (by definition)
  // Nil
  // (by definition)
  // fmap g Nil
  // (by definition)
  // fmap g gmap f Nil
  //
  // fmap (g . f) (Cons h t)
  // (by definition)
  // Cons (g . f h) (fmap (g . f) t)
  // (by proposition)
  // Cons (g . f h) ((fmap g) . (fmap f) t)
  // (by definition in reverse)
  // fmap g (Cons (f h) (fmap f t))
  // (by definition in reverse)
  // fmap g . fmap f (Cons h t)
}
