import java.util.Optional;
import java.util.function.Function;

/**
 * Solutions for Chapter 4 of "Category Theory for Programmers".
 */
public class aloussase_solutions {

// Ex 1
public static <T,U,V> Function<T, Optional<V>>
compose(Function<T, Optional<U>> f, Function<U, Optional<V>> g) {
    return (T x) -> f.apply(x).flatMap(y -> g.apply(y));
}

// Ex 2
public static Optional<Double> safeReciprocal(int x) {
    return x > 0 ? Optional.of(1.0 / x) : Optional.empty();
}

public static Optional<Double> safeSqrt(double x) {
    return x >= 0 ? Optional.of(Math.sqrt(x)) : Optional.empty();
}

// Ex 3
public static Optional<Double> safeRootReciprocal(int x) {
    return compose(
            aloussase_solutions::safeReciprocal,
            aloussase_solutions::safeSqrt
        ).apply(x);
}

public static void main(String... args) {
    assert safeRootReciprocal(1).equals(Optional.of(1.0));
    assert safeRootReciprocal(4).equals(Optional.of(Math.sqrt(0.25)));
    assert safeRootReciprocal(0).equals(Optional.empty());
}

}
