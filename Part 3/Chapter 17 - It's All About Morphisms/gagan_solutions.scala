object Solution {

  /**
    * 1. Consider some degenerate cases of a naturality condition and draw the appropriate diagrams. For instance,
    * what happens if either functor 𝐹 or 𝐺 map both objects 𝑎 and 𝑏 (the ends of 𝑓 ∷𝑎→𝑏) to the same object,
    * e.g.,𝐹𝑎 = 𝐹𝑏 or 𝐺𝑎 = 𝐺𝑏? (Notice that you get a cone or a co-cone this way.) Then consider cases where either
    * 𝐹𝑎 = 𝐺𝑎 or 𝐹𝑏 = 𝐺𝑏. Finally, what if you start with a morphism that loops on itself — 𝑓 ∷ 𝑎 → 𝑎?
    */
  //
  // if either functor 𝐹 or 𝐺 map both objects 𝑎 and 𝑏 (the ends of 𝑓 ∷ 𝑎 → 𝑏) to the same object
  //
  // case 1: Fa = Fb
  //
  //          Ff (loop)  alphaA
  //    a ------------> Ga
  //    |  \         ↑   |
  //    |   ↓       /    |
  // f  |   Fa (= Fb)    | Gf
  //    |  ↑         \   |
  //    ↓ /           ↓  ↓
  //    b------------> Gb
  //
  // alpha :: for all x, Fx -> Gx
  //
  // From  naturality condition, Gf . alphaA = alphaB . Ff
  // (Ga -> Gb) . (Fa -> Ga) = (Fb -> Gb) . (Fa -> Fb)
  //
  // Given natural transformation between F and G exist, this would mean the definition is irrespective of the type,
  // the type comes later. Then since this N.T. would be a pure function, for the same input, we should get the same output.
  //
  // alphaA :: Fa -> Ga
  // alphaB :: Fb -> Gb
  //
  // Both given the same value Fa, yields Ga and Gb respectively, implying Ga == Gb.
  // We get a cone, instead of a naturality square. Cones are just natural transformations.
  //
  // case 2: Ga = Gb
  //
  //    a ------------> Fa
  //    |               | \
  //    |               |  ↓
  // f  |            Ff |  Ga (=Gb) Gf (loop)
  //    |               | ↑
  //    ↓               ↓/
  //    b-------------> Fb
  //
  // But having same output gives no guarantees about the input. Thus we cannot say anything about Fa and Fb.
  // We get a co-cone, instead of a naturality square.
  //
  // case 3: Fa = Ga
  //    a ------------> Fa(=Ga)
  //    |               | \
  //    |               |  \
  // f  |            Ff |   \ Gf
  //    |               |    \
  //    ↓               ↓     ↓
  //    b-------------> Fb--> Gb
  //
  // since Fa == Ga, this would mean both F and G are the same containers, given alphaA = id, following the same logic
  // as case 1, implementation irrespective of type, we can conclude F == G.
  //
  // case 4: Fb = Gb
  //    a ------------> Fa -----> Ga
  //    |               |         /
  //    |               |        /
  // f  |            Ff |       /   Gf
  //    |               |      /
  //    ↓               ↓     ↓
  //    b-------------> Fb=(Gb)
  //
  // since Fb == Gb, this would mean both F and G are the same containers, given alphaB = id, following the same logic
  // as case 1, implementation irrespective of type, we can conclude F == G.
  //
  // case 5: f:: a -> a
  // then Fa = Fb and Ga = Gb
  // Ff = id
  // Gf = id
  //
  // alphaA = alphaB
  // Though still nothing can be said about F and G.
  //

}
