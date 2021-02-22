import scala.Option
import scala.language.postfixOps

object Main extends App {

    // book code 
    def safe_root(x : Double): Option[Double] = if (x >= 0) Some(scala.math.sqrt(x)) else None

    assert(safe_root(-5.0) == None)
    assert(safe_root(64.0).get == 8.0)

    // Q2
    def safe_reciprocal(x: Double): Option[Double] = if (x != 0) Some(1.0 / x) else None

    assert(safe_reciprocal(0) == None)
    println(safe_reciprocal(0.01).get)

    // Q1 Kleisli Category
    def compose_optionals(f: Double => Option[Double], g: Double => Option[Double], x : Double): Option[Double] = {
        val res1 = f.apply(x)
        val res2 = if (res1 != None) g.apply(res1.get) else None
        return res2
    }

    // Q3

    def safe_root_reciprocal(x : Double) : Option[Double] = compose_optionals(safe_reciprocal, safe_root, x)

    assert(safe_root_reciprocal(-2.0) == None)
    assert(safe_root_reciprocal(0) == None)
    assert(safe_root_reciprocal(0.01).get == 10)
}