import challenges._23_Comonads.Solution.Store.PairStore

object Solution {

  /**
    * Implement the Conway’s Game of Life using the Store comonad.
    * Hint: What type do you pick for s?
    */
  
  // type s = (Int, Int)
  // type a = Option[Int]
  
  /**
    * These rules, which compare the behavior of the automaton to real life, can be condensed into the following:
    *
    * Any live cell with two or three neighbors survives.
    * Any dead cell with three live neighbors becomes a live cell.
    * All other live cells die in the next generation. Similarly, all other dead cells stay dead.
    */
  def computeNextStateValue(g: GridStore[Option[Int]]): Option[Int] = {
    val (neighbours, value) = (g.getAllNeighbourValues, g.value)
    val sum = neighbours.flatten.sum
    val result = (sum, value) match {
      case (_, None)                        => None
      case (s, Some(1)) if s == 2 || s == 3 => Some(1)
      case (3, Some(0))                     => Some(1)
      case _                                => Some(0)
    }
    result
  }

  def nextGrid(grid: GridStore[Option[Int]]): GridStore[Option[Int]] =
    GridStore.GridStoreCoMonad.coFlatMap(grid)(computeNextStateValue)

  def main(args: Array[String]): Unit = {
    val board = Vector(Vector(0, 1, 0), Vector(1, 0, 0), Vector(0, 0, 1))

    def isValid(i: Int) = 0 <= i && i < board.length

    def fetch(r: Int, c: Int): Option[Int] = {
      if (isValid(r) && isValid(c)) Some(board(r)(c)) else None
    }

    val initial: GridStore[Option[Int]] = GridStore(Store({
      case (r, c) => fetch(r, c)
    }, (0, 0)))

    (1 to 5).foldLeft(initial)((acc, i) => {
      println(s"Generation $i\n${pretty(
        (0 to 2).map(r => (0 to 2).map(c => acc.underlying.f((r, c)).get)))}")
      nextGrid(acc)
    })
  }

  private def pretty(lss: Seq[Seq[Int]]): String = {
    lss.map(_.mkString(" ")).mkString("\n")
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait CoMonad[F[_]] extends Functor[F] {
    def extract[A](fa: F[A]): A

    def coFlatten[A](fa: F[A]): F[F[A]]

    def coFlatMap[A, B](fa: F[A])(f: F[A] => B): F[B] = map(coFlatten(fa))(f)
  }

  case class Store[S, A](f: S => A, s: S) {
    def current: A = f(s)
  }

  object Store {
    type Pair = (Int, Int)
    type PairStore[A] = Store[Pair, A]

    implicit val coMonad: CoMonad[PairStore] = new CoMonad[PairStore] {
      override def extract[A](fa: PairStore[A]): A = fa.f(fa.s)

      override def coFlatten[A](fa: PairStore[A]): PairStore[PairStore[A]] =
        Store[Pair, Store[Pair, A]](s => Store(fa.f, s), fa.s)

      override def map[A, B](fa: PairStore[A])(f: A => B): PairStore[B] =
        Store[Pair, B](fa.f andThen f, fa.s)
    }
  }

  case class GridStore[A](underlying: PairStore[A]) {

    def value: A = underlying.current

    def north: GridStore[A] = {
      val (r, c) = underlying.s
      GridStore(Store(underlying.f, (r - 1, c)))
    }

    def south: GridStore[A] = {
      val (r, c) = underlying.s
      GridStore(Store(underlying.f, (r + 1, c)))
    }

    def east: GridStore[A] = {
      val (r, c) = underlying.s
      GridStore(Store(underlying.f, (r, c + 1)))
    }

    def west: GridStore[A] = {
      val (r, c) = underlying.s
      GridStore(Store(underlying.f, (r, c - 1)))
    }

    def getAllNeighbourValues: List[A] =
      List(
        north,
        south,
        east,
        west,
        north.west,
        north.east,
        south.west,
        south.east
      ).map(_.value)
  }

  object GridStore {

    import Store.coMonad

    implicit val GridStoreCoMonad: CoMonad[GridStore] = new CoMonad[GridStore] {
      override def extract[A](fa: GridStore[A]): A =
        coMonad.extract(fa.underlying)

      override def coFlatten[A](fa: GridStore[A]): GridStore[GridStore[A]] = {
        GridStore(coMonad.coFlatMap(fa.underlying)(GridStore(_)))
      }

      override def map[A, B](fa: GridStore[A])(f: A => B): GridStore[B] =
        GridStore(coMonad.map(fa.underlying)(f))
    }
  }

}
