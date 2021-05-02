object Solution {

  /**
    * 1. How would you describe a pushout in the category of C++ classes?
    */
  //      z
  //      ↑
  //     /|\
  //    | a |
  //    | ↑ |
  //    |/|\|
  //    b | c
  //    ↑ | ↑
  //     \|/
  //      d
  //
  // if d inherits from both b and c (pattern 1 <- 2 -> 3)
  // b and c and d, inherit from a, then `a` be the CoLimit.
  // and if b and c inherit from some z then a also inherits from z (can be safely casted).
  //
  // now pushout from d, two morphism we want equate, d -> b and d -> c, same domain but different co-domain
  // `a` be the pushout, lowest common ancestor.
  //
  // pushout: any class z that both b and c inherit from, then `a` also inherits from that class

  /**
    * 2. Show that the limit of the identity functor Id ∷ 𝐂 → 𝐂 is the initial object
    */
  // For Limit to exist there must be projections (component of natural transformations) from apex c
  // to every object that the functor D (ID in this case) mapped from I (pattern category, C in this case) to C.
  //
  // And by definition initial object has 1 morphism to every other object. Any other object that also has 1 morphism to
  // every other object is isomorphic to the initial object. Thus initial object is the most general hence the Limit.

  /**
    * 3. Subsets of a given set form a category. A morphism in that category is
    * defined to be an arrow connecting two sets if the first is the subset of the second.
    * What is a pullback of two sets in such a category?
    * What’s a pushout?
    * What are the initial and terminal objects?
    */
  //      z
  //      ↑
  //     /|\
  //    | a |
  //    | ↑ |
  //    |/ \|
  //    b   c
  //    ↑   ↑
  //    |\ /|
  //    | d |
  //    | ↑ |
  //     \|/
  //      e
  //
  // initial object: empty set, subset of all, one arrow from it to every subset.
  // final object: full set, union of all, every other set is a subset of the full set, one arrow from all to full set.
  //
  // pullback be `d` (intersection): for the pattern b -> a, c -> a, wherein both b & c be subset of `a`, and d be subset
  // of both b & c, then d would be the most general (Limit) if it was the intersection of b & c.
  //
  // pushout be `a` (union): for the pattern d -> b and d -> c, d be the subset of both b and c, then `a` be the common superset.
  // `a` would be most general (CoLimit) if it's the union of b and c. There would be a morphism from `a` to all other supersets.
  //

  /**
    * 4. Can you guess what a coequalizer is?
    */
  // f :: a -> b
  // g :: a -> b
  //
  // p :: a -> c
  // q :: b -> c
  //
  // q . f == q . g
  //
  // if we restrict our attention to Set, then the image of the function q selects a subset of b, when restricted to this
  // subset elements mapped through functions f and g are equal.
  //
  // quotient type: is an algebraic data type that represents a type whose equality relation has been redefined by a given
  // equivalence relation such that the elements of the type are partitioned into a set of equivalence classes whose cardinality
  // is less than or equal to that of the base type.
  //
  // equalizer represents refinement type while co-equalizer represents quotient type, Scala or Haskell don't have quotient types.
  //

  /**
    * 5. Show that, in a category with a terminal object, a pullback towards the
    * terminal object is a product.
    */
  //
  //      a
  //      ↑
  //     /|\
  //    b | c
  //    ↑ | ↑
  //    |\|/|
  //    | d |
  //    | ↑ |
  //     \|/
  //      d'
  //
  // if a == terminal object, then d has to (b, c)
  // f :: b -> a
  // f _ = a
  //
  // g :: c -> a
  // g _ = a
  //
  // m :: d' -> d
  //
  // d be the Limit, best candidate would be the product
  // p :: d -> b
  // p d = first d
  //
  // q :: d -> c
  // q d = second d
  //
  // f and g by definition of terminal object are unique, also terminal object in I maps to terminal object in C
  // This category of 3 objects is isomorphic to 2 category, hence the Limit is the same as the Limit of the 2 category.
  //
  
  /**
    * 6. Similarly, show that a pushout from an initial object (if one exists) is the
    * coproduct
    */
  //
  //      z
  //      ↑
  //     /|\
  //    | a |
  //    | ↑ |
  //    |/|\|
  //    b | c
  //    ↑ | ↑
  //     \|/
  //      d
  //
  // if d == initial object, then a has to Either b c
  // f :: d -> b
  // f _ = element of b
  //
  // g :: d -> c
  // g _ = element of c
  //
  // m :: a -> z
  //
  // d be the CoLimit, best candidate would be the coproduct
  //
  // f and g by definition of initial object are unique, also initial object in I maps to initial object in C
  // This category of 3 objects is isomorphic to 2 category, hence the CoLimit is the same as the CoLimit of the 2 category.
  // The CoLimit of this category is the coproduct, so the pushout towards the initial object is the coproduct.
  //
}
