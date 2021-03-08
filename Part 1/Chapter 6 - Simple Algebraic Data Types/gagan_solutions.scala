object Solution {

  /**
    * 1. Show the isomorphism between Maybe a and Either () a.
    */
  def fromMaybe[A](m: Option[A]): Either[Unit, A] = m match {
    case Some(value) => Right(value)
    case None        => Left(())
  }

  def fromEither[A](e: Either[Unit, A]): Option[A] = e match {
    case Right(value) => Some(value)
    case Left(_)      => None
  }

  /**
    * 2. Here’s a sum type defined in Haskell:
    * When we want to define a function like area that acts on a Shape, we do it by pattern matching on the two constructors:
    * data Shape = Circle Float
    * | Rect Float Float
    * area :: Shape -> Float
    * area (Circle r) = pi * r * r
    * area (Rect d h) = d * h
    *
    * Implement Shape in C++ or Java as an interface and create two classes:
    * Circle and Rect. Implement area as a virtual function.
    */
  sealed trait Shape
  case class Circle(r: Double) extends Shape
  case class Rectangle(l: Double, b: Double) extends Shape

  /**
    * 3. Continuing with the previous example: We can easily add a new function circ that calculates the circumference of a Shape.
    * We can do it without touching the definition of Shape:
    *
    * circ :: Shape -> Float
    * circ (Circle r) = 2.0 * pi * r
    * circ (Rect d h) = 2.0 * (d + h)
    *
    * Add circ to your C++ or Java implementation. What parts of the original
    * code did you have to touch?
    */
  def area(s: Shape): Double = s match {
    case Circle(r)       => math.Pi * r * r
    case Rectangle(l, b) => l * b
  }

  def circ(s: Shape): Double =
    s match { // without touching definition of Shape
      case Circle(r)       => 2 * math.Pi * r
      case Rectangle(l, b) => 2 * (l + b)
    }

  /**
    * 4. Continuing further: Add a new shape, Square, to Shape and make all the
    * necessary updates. What code did you have to touch in Haskell vs. C++ or Java? (Even if you’re not a Haskell programmer,
    * the modifications should be pretty obvious.)
    */
  case class square(l: Double) extends Shape

  def area2(s: Shape): Double = s match {
    case Circle(r)       => math.Pi * r * r
    case Rectangle(l, b) => l * b
    case square(l)       => l * l
  }

  def circ2(s: Shape): Double =
    s match {
      case Circle(r)       => 2 * math.Pi * r
      case Rectangle(l, b) => 2 * (l + b)
      case square(l)       => 4 * l
    }

  // had to touch pattern cases

  /**
    * 5. Show that 𝑎 + 𝑎 = 2 × 𝑎 holds for types (up to isomorphism). Remember that 2 corresponds to Bool, according to our translation table.
    */
  def fromEither[A](e: Either[A, A]): (Boolean, A) = e match {
    case Left(value)  => (false, value)
    case Right(value) => (true, value)
  }

  def toEither[A](p: (Boolean, A)): Either[A, A] =
    if (p._1) Right(p._2) else Left(p._2)

}
