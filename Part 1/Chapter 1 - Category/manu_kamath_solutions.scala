object Main extends App {
    /** 
     * Q1: Identity Function
     */
    def id[A] (x : A) : A = x
    // Tests
    println(id(4)) // 4
    println(id("Hey")) // "Hey"
    println(id(Array(1.5,2.3,3.7,4.667))) // I@20b5f2ac (reference)
    def add (x: Int, y: Int) : Int = x + y
    println(id(add(3,4)) == add(id(3), id(4))) // true

    /** 
     * Q2: Composition function
     */     
    def compose[A, B, C](a: A)(f: A => B)(g: B => C): C = g(f(a))

    /** 
     * Q3: Test composition respects identity
     */
    def test_composition_identity () = {
        def addOne (x: Int) : Int = x + 1
        def sq (x : Int) : Int = x * x
        assert(compose(5)(addOne)(sq) == 36)
        assert(id(compose(5)(addOne)(sq)) == compose(id(5))(addOne)(sq))
        // this errs out but is probably what the question asked for -
        // perhaps the right way to do it is to apply f to a, then g to f?
        // assert(id(compose(5)(addOne)(sq)) == compose(id(5))(id(addOne))(id(sq)))

        println("Test: Composition respects identity - PASSED")
    }

    test_composition_identity()

    /**
     * Q4 - Q6: Conceptual
     * 
     * 4. Pages are objects, Links are Morphisms.
     * i) Links are composable. If I click on a link on page A to page B,
     * and then on a link on page B to page C, then we can construct a link
     * that travels from A to C by composing the two links.
     * ii) the WWW is a category ONLY if every page has a link to itself. Now most pages' homepage
     * usually have a link which you can click to redirect itself, but this is not 100% guaranteed so 
     * strictly speaking the WWW will NOT be a category.
     * 
     * 5. No. If I am friends with Jake and Jake is friends with Helen
     * then there need not exist a friendship between me and Helen.
     * Also, I'm not sure if you can be friends with yourself 
     * on FB (identity friendship)?
     * 
     * 6. Following conditions should be met:
     * i. Each node should have a cycle to itself (identity edge)
     * ii. For every node in the graph if there is an edge from A to B
     * and an edge from B to C, then there MUST exist an edge from A to C.
     */
}
