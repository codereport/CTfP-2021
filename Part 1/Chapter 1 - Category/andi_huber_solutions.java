import java.util.function.*;

import org.junit.jupiter.api.Test;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

@SuppressWarnings({"rawtypes", "unchecked"}) // I know, I know ... this is cheating
public class andi_huber_solutions {

    @Test
    void compositionWithIdentity() {
        
        // 1. identity operator
        
        var id = UnaryOperator.identity();
        
        // 2. function composition
        
        BinaryOperator<Function> compose = (f, g) -> f.compose(g);
        
        // 3. define test predicate
        
        BiPredicate<UnaryOperator, Object> test = 
                (f, x) -> compose.apply(f, id).apply(x) == f.apply(x);
        
        // test with arbitrary operator eg. (x -> x * 3)
                
        UnaryOperator times3 = x -> (int)x * 3;
        
        assertTrue(test.test(times3, 15));
        
    }
}
    
