import java.util.function.Function;
import java.util.stream.Stream;

public class aloussase_solutions {

// 1.4.1
public static <T> T identity(T a) {
    return a;
}

// 1.4.2
public static <A, B, C> Function<A, C> compose(Function<A, B> f, Function<B, C> g) {
    // return g.compose(f);     Using Function methods
    // return f.andThen(g);
    return (A a) -> g.apply(f.apply(a));
}

public static void main(String... args) {
    /* Silly example. Counts lines in a String and show that identity works. */
    Function<String, Long> nlines = compose(aloussase_solutions::identity,
                                            compose((String s) -> s.lines(),
                                                     (Stream<String> st) -> st.count()));
    System.out.println(nlines.apply("This\nis\nan\nexample"));
}

}
