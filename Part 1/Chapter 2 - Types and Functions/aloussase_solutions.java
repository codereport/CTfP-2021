import java.util.HashMap;
import java.util.function.Function;
import java.util.function.Predicate;

public class aloussase_solutions {

/* Ex 1: memoize function */
public static <T,R> Function<T, R> memoize(Function<T, R> f) {
    var cache = new HashMap<T, R>();
    return (T x) -> cache.computeIfAbsent(x, k -> f.apply(k));
}

public static int noisyInc(int n) {
    System.out.printf("Incrementing %d...\n", n);
    return n + 1;
}

public static void main(String... args) {
    /* Ex 1 Test */
    var memoizedInc = memoize(aloussase_solutions::noisyInc);

    System.out.println(memoizedInc.apply(1)); /* prints "Incrementing 1..." */
    System.out.println(memoizedInc.apply(1)); /* does not print "Incrementing 1..." */

    /* Ex 5: functions from Bool to Bool */
    Predicate<Boolean> booleanId = b -> b;
    Predicate<Boolean> not       = booleanId.negate();
    Predicate<Boolean> True      = b -> true;
    Predicate<Boolean> False     = b -> false;

    /* Ex 5 Test */
    System.out.println(not.test(booleanId.test(True.test(true)))); /* prints "false" */
}

}
