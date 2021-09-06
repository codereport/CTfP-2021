object Solution {

  /**
    * 1. What is the action of the free functor 𝐹 ∷ 𝐶 → 𝐶𝑇 on morphisms.
    * Hint: use the naturality condition for monadic 𝜇.
    */
  //
  // CT: Eilenberg-Moore Category, category of T-Algebras, algebras compatible with Monad T
  // T-Algebra: (a, f), pair of carrier object and the evaluator (F-Algebra)
  //
  // R, Forgetful Functor UT, CT -> C: maps (a, f) to a
  //
  // L, Free Functor FT, C -> CT: maps a in C, to a T-Algebra in CT, (Ta, μa)
  // FT a = (T a, μa)
  //
  // Since the evaluator for Free Functor is the component of natural transformation at a, μa, we use the naturality condition
  // between Functors T (T) and T
  //
  // μa:: T (T a) -> T a
  // μb:: T (T b) -> T b
  //
  // for T (T a) to T b, we have two equivalent paths
  // fmapT . μa = μb . fmapTT
  //
  // for morphishm f:: a -> b, in C
  // fmap:: (a -> b) -> FT a -> FT b
  //
  // we have, fmap f (T a, μa) = (fmapT f (T a), fmapT . μa)
  //

  /**
    * 2. Define the Adjunction: 𝑈w ⊣ 𝐹w
    */
  //
  // for a CoAlgebra we have, coeval:: a -> f a
  //
  // for a CoMonad W, we have,
  // counit, ∊:: W a -> a
  // duplicate, δ:: W a -> W (W a)
  //
  // For CoMonad W, we have category of CoAlgebras Cw compatible with it
  //
  // L, Forgetful Functor, Uw, forgets the coeval, (W a, δa) -> W a
  // R, CoFree Functor, Fw, map object a -> ((W a, δa)), generates CoFree CoAlgebras
  //
  //          Uw
  //      <---------
  //    C           Cw
  //      --------->
  //          Fw
  //
  // Uw ⊣ Fw
  //
  // η:: Id -> R . L
  // η:: Icw -> Fw . Uw
  //
  // ε:: L . R -> Ic
  // ε:: Uw . Fw -> Ic
  //
  // Triangular Identities
  //
  // Uw = Uw . Icw -> Uw . Fw . Uw -> Ic . Uw = Uw
  // Fw = Icw . Fw -> Fw . Uw . Fw -> Fw. Ic = Fw
  //
  //

  /**
    * 3. Prove that the above adjunction reproduces the original comonad.
    */
  // Fw . Uw to reproduce the original CoMonad
  //
  // R = Fw, L = Uw
  //
  // Considering unit of the adjunction, η:: I -> R . L
  // We apply object (W a, δa),
  // Uw forgets the coeval part yielding W a, afterwards Fw maps, W a to (W (W a), δ(W a))
  //
  // Since coeval stays the same, we essentially have a mapping from W a to W (W a), which is the `duplicate` of CoMonad.
  // Thus unit of the adjunction is the `duplicate` of the CoMonad
  //
  // Considering counit of the adjunction, ε:: L . R -> I
  // We take component of this natural transformation at object a in Category C
  // Fw lifts a to (W a, δa), then Uw forgets the coeval yielding W a, I at a gives back a.
  // Thus counit reduces to W a -> a, which is the counit of the CoMonad as expected.
  //
}
