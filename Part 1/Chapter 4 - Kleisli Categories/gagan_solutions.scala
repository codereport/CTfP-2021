object Solution {

  // 1. Construct the Kleisli category for partial functions (define composition and identity).
  
  def compose[A, B, C](f: A => Option[B], g: B => Option[C]): A => Option[C] = {
    (a: A) =>
      {
        val fa = f(a)
        if (fa.isDefined) g(fa.get) else Option.empty
      }
  } // or f(_) flatMap g

  def identity[A]: A => Option[A] = Option(_) // or Some(_)

  // 2. Implement the embellished function safe_reciprocal that returns a valid reciprocal of its argument, if it’s different from zero.
  
  val safeReciprocal: Double => Option[Double] =
    (r: Double) =>
      if (r != 0) Option(1 / r) else Option.empty // or use Some(1/r) and None

  val safeRoot: Double => Option[Double] =
    (r: Double) => if (r >= 0) Option(math.sqrt(r)) else Option.empty

  // 3. Compose the functions safe_root and safe_reciprocal to implement safe_root_reciprocal that calculates sqrt(1/x) whenever possible.
  
  val safeRootReciprocal: Double => Option[Double] =
    (r: Double) => compose(safeReciprocal, safeRoot)(r) // or compose(safeReciprocal, safeRoot)(_)

}
