object Solution {

  /**
  * 1. Consider a free monoid built from a singleton set as its generator. Show that there is a one-to-one correspondence
  * between morphisms from this free monoid to any monoid 𝑚, and functions from the singleton set to the underlying set of 𝑚.
  */
  // Let this singleton set be Unit ().
  // We know that a free monoid generated from a free functor F, aggregates the arguments passed to it.
  //
  // free monoid built from just one generator. It’s the type of the list of units, [()]. Its elements are [], [()], [(), ()],etc.
  // Every such list can be described by one natural number — its length. There is no more information encoded in the list of units.
  // Appending two such lists produces a new list whose length is the sum of the lengths of its constituents.
  // It’s easy to see that the type [()] is isomorphic to the additive monoid of natural numbers (with zero).
  //
  // x = {()}, then F x = [()], list of unit encodes within itself the length, one natural number.
  // for any monoid m, and it's underlying set U m
  //
  // From Free -| Forgetful adjunction we have, 1:1 correspondence, the isomorphism of Hom-sets:
  // Mon([()], m) ≅ Set({()}, U m)
  //
  // From RHS,
  // a morphism `g` in Hom-set ({()}, U m), corresponds to an element of U m, for source is a singleton set
  // this element be corresponding to unit (), denoted as g ()
  //
  // From LHS,
  // every element of type [()] corresponds to a natural number, enumeration. A morphism 'f' in Hom-set Mon([()], m),
  // corresponds to enumerating/ indexing m.
  // since f is a homomorphism (maps unit to unit and preserves structure i.e), we have
  // f [] = unit element of m
  // f [()] = g ()
  // then f [(), ()] = g () + g (), where + is the monoid operator for m
  //
  // if we start from Set, we take one element from underlying set of m, then the corresponding homomorphism be wherein
  // f maps [()] to this element, other elements are mapped given the rules of homomorphism.
  //
  // similarly if we start from Mon, a homomorphism, them the element mapped corresponding to [()] in f, would be the element
  // selected on g in the right, for the right there's function from singleton set to underlying set of m, a selection function,
  // just like initial object.
  //
  // Thus we have 1:1 correspondence between morphisms from this free monoid to any monoid 𝑚, and functions from the
  // singleton set to the underlying set of 𝑚.
  //
  //
}
