object Solution {

  /**
    * 1. Derive the naturality square for 𝜓, the transformation between the two (contravariant) functors:
    * d → 𝐂(𝐿d, c) d → 𝐃(d, 𝑅c)
    */
  //
  // L -| R, L is left adjoint to R
  //
  // Functor F[d] = L(d) -> c, given object c in category C, Functor F maps category D to Set, maps an object in D to a
  // set of functions between two objects of Category C.
  // Functor G[d] = d -> R(c), given object c in category C, Functor G maps category D to Set maps an object in D to a
  // set of functions between two objects of Category D.
  //
  // Let's see now how do they map a morphism in category D
  //
  // f:: d1 -> d2
  //
  // F :: (d1 -> d2) -> (L(d2) -> c) -> (L(d1) -> c)
  // F f ld2c = ld2rc . Lf
  //
  // G :: (d1 -> d2) -> (d2 -> R(c)) -> (d1 -> R(c))
  // G f d2rc = d2rc . f
  //
  // ψ :: F ~> G
  // ψ d :: (L(d) -> c) -> (d -> R(c))
  //
  // Then the naturality square be, F[d2] => G[d1] or ((L(d2) -> c) -> (d1 -> R(c)))
  // Gf . ψ d2 = ψ d1 . F f
  //
  // LHS
  // Gf . ψ d2
  // ((d2 -> R(c)) -> (d1 -> R(c))) . ((L(d2) -> c) -> (d2 -> R(c))) :: ((L(d2) -> c) -> (d1 -> R(c)))
  // applying F d2 :: (L(d2) -> c)
  // (Gf . ψ d2) (F d2) --- (1.)
  // Gf (ψ d2 (F d2))
  // Gf (d2 -> R(c))
  // Gf (G d2)
  // G d1
  // now from natural transformation, G d1 = ψ d1 (F d1)
  // ψ d1 (F d1)
  // ψ d1 (F f (F d2))
  // (ψ d1 . F f) (F d2) --- (1.)
  //
  // RHS
  // ψ d1 . F f
  // ((L(d1) -> c) -> (d1 -> R(c))) . ((L(d2) -> c) -> (L(d1) -> c)) :: ((L(d2) -> c) -> (d1 -> R(c)))
  // applying F d2 :: (L(d2) -> c)
  // (ψ d1 . F f) (F d2) --- (2.)
  // ψ d1 (F f (F d2))
  // ψ d1 (F d1)
  // G d1
  // Gf (G d2)
  // Gf (d2 -> R(c))
  // Gf (ψ d2 (F d2))
  // (Gf . ψ d2) (F d2) --- (2.)
  //
  //
  // ψ' :: G ~> F
  // ψ' d :: (d -> R(c)) -> (L(d) -> c)
  //
  // Then the naturality square be, G[d2] => F[d1] or ((d2 -> R(c)) -> (L(d1) -> c))
  // F f . ψ' d2 = ψ' d1 . G f
  //
  // LHS
  // F f . ψ' d2
  // (L(d2) -> c) -> (L(d1) -> c) . (d2 -> R(c)) -> (L(d2) -> c) :: ((d2 -> R(c)) -> (L(d1) -> c))
  // applying G d2 :: (d2 -> R(c))
  // (Ff . ψ' d2) (G d2) --- (3.)
  // Ff (ψ' d2 (G d2))
  // Ff (L(d2) -> c)
  // Ff (F d2)
  // F d1
  // ψ' d1 (G d1)
  // ψ' d1 (G f (G d2))
  // (ψ' d1 . G f) (G d2) --- (3.)
  //
  // RHS
  // ψ' d1 . G f
  // ((d1 -> R(c)) -> (L(d1) -> c)) . (d2 -> R(c)) -> (d1 -> R(c)) :: ((d2 -> R(c)) -> (L(d1) -> c))
  // applying G d2 :: (d2 -> R(c))
  // (ψ' d1 . G f) (G d2) --- (4.)
  // ψ' d1 (G f (G d2))
  // ψ' d1 (G d1)
  // F d1
  // Ff (F d2)
  // Ff (L(d2) -> c)
  // Ff (ψ' d2 (G d2))
  // (Ff . ψ' d2) (G d2) --- (4.)
  //

  /**
    * 2. Derive the counit 𝜀 starting from the hom-sets isomorphism in the second definition of the adjunction.
    */
  // counit
  // 𝜀 :: (L . R) -> Ic
  //
  // Hom-set isomorphism C(L(d), c) ≅ D(d, R(c)), natural in c and d
  //
  // Since this isomorphism works for any object d, it must also work for d = R(c)
  // C(L(R(c)), c) ≅ D(R(c), R(c))
  // C((L.R)(c)), c) ≅ D(R(c), R(c))
  // RHS must contain at-least one morphism the identity
  // For LHS,
  // C((L.R)(c)), c)
  // C((L.R)(c)), Ic), family of morphisms parameterized by c, component of Natural Transformation at c
  // between functors L . R and Ic, which is exactly counit 𝜀
  //

  /**
    * 3. Complete the proof of equivalence of the two definitions of the adjunction.
    */
  //
  // Definition #1: Given two functor L and R going going back and forth between categories C and D, with
  // Natural transformations η and 𝜀, such that the triangular identities are satisfied
  //
  // counit
  // 𝜀 :: (L . R) -> Ic
  //
  // unit
  // η :: Id -> (R . L)
  //
  // Definition #2: Functor L is left adjoint to Functor R iff C(L(d), c) ≅ D(d, R(c))
  // If this mapping is invertible and can be naturally extended across all Hom-sets, we have an adjunction.
  //
  // Deriving Definition #1 From Definition #2
  //
  // C(L(d), c) ≅ D(d, R(c))
  //
  // unit
  // substituting c = L(d)
  // C(L(d), L(d)) ≅ D(d, R(L(d)))
  // LHS must contain at-least one morphism the identity
  // C(L(d), L(d)) ≅ D(d, R(L(d)))
  // RHS
  // D(Id, ((R . L) d)), family of morphisms parameterized by d, component of Natural Transformation at d
  // between functors Id and R . L, which is exactly unit η
  //
  // counit
  // substituting d = R(c)
  // C(L(R(c)), c) ≅ D(R(c), R(c))
  // C((L.R)(c)), c) ≅ D(R(c), R(c))
  // RHS must contain at-least one morphism the identity
  // For LHS,
  // C((L.R)(c)), c)
  // C((L.R)(c)), Ic), family of morphisms parameterized by c, component of Natural Transformation at c
  // between functors L . R and Ic, which is exactly counit 𝜀
  //
  //
  // Deriving Definition #2 From Definition #1
  //
  // C(L(d), c) ≅ D(d, R(c))
  //
  // for a morphism f :: L(d) -> c, we want to derive a corresponding morphism d -> R(c)
  // then R f :: R(L(d)) -> R c
  // given unit at d, ηd :: d -> (R . L)d
  // then
  // ϕ :: (L(d) -> c) -> (d -> R(c))
  // ϕ f = R f . ηd
  //
  // for a morphism g :: d -> R(c), we want to derive a corresponding morphism L(d) -> c
  // then L g :: L(d) -> L(R (c))
  // given counit at c, 𝜀c :: (L . R)c -> c
  // then
  // ψ :: (d -> R(c)) -> (L(d) -> c)
  // ψ g = 𝜀c . L g
  //
  // Thus we derive the 1:1 correspondence between Hom-sets
  //

  /**
    * 4. Show that the coproduct can be defined by an adjunction. Start with the
    * definition of the factorizer for a coproduct.
    */
  //
  // factorizer :: (a -> c) -> (b -> c) -> Either a b -> c
  //
  //          coproduct
  // (a | b) <------------ <a, b>
  //    |                |
  //    |                |
  // m  |                | <p, q>
  //    |                |
  //    ↓       Δc       ↓
  //    c ------------>  <c, c>
  //
  // coporduct is then defined as the left adjoint of the diagonal functor
  // L = coproduct
  // R = Δc
  //
  // (C x C) (a | b, c) ≅ C(<a, b>, Δc)
  //
  // For the full adjunction, the factorizer need be invertible
  // p = m . Left(_)
  // q = m . Right(_)
  //

  /**
    * 5. Show that the coproduct is the left adjoint of the diagonal functor.
    */
  // same as #4

  /**
    * 6. Define the adjunction between a product and a function object in Haskell.
    */
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Adjunction[F[_], G[_]] {
    val F: Functor[F]
    val G: Functor[G]

    def leftAdjoint[A, B](p: F[A] => B): A => G[B]
    def rightAdjoint[A, B](q: A => G[B]): F[A] => B
  }

  type X

  type Product[Y] = (Y, X)
  type Exponential[Y] = X => Y

  val ProductExponentialAdjunction: Adjunction[Product, Exponential] =
    new Adjunction[Product, Exponential] {

      val F: Functor[Product] = new Functor[Product] {
        override def map[A, B](fa: (A, X))(f: A => B): (B, X) =
          (f(fa._1), fa._2)
      }
      val G: Functor[Exponential] = new Functor[Exponential] {
        override def map[A, B](fa: X => A)(f: A => B): X => B = f compose fa
      }

      override def leftAdjoint[A, B](p: ((A, X)) => B): A => X => B =
        a => x => p(a, x)

      override def rightAdjoint[A, B](q: A => X => B): ((A, X)) => B = {
        case (a, x) => q(a)(x)
      }
    }

}
