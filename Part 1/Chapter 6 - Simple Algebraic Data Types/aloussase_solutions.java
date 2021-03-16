public class aloussase_solutions {

    /* Exercise 2 */
    sealed interface Shape permits Circle, Rectangle, Square {
        double area();
        double circ();
    }

    record Circle(double radius) implements Shape {
        public double area() { return Math.pow(radius, 2) * Math.PI; }
        public double circ() { return 2 * radius * Math.PI; }
    }

    record Rectangle(double width, double height) implements Shape {
        public double area() { return width * height; }
        public double circ() { return (width + height) * 2; }
    }

    /* Exercise 4 */
    record Square(double side) implements Shape {
        public double area() { return Math.pow(side, 2); }
        public double circ() { return 4 * side; }
    }

    public static void printShape(Shape shape) {
        System.out.printf("%-35s area=%-8.2f circ=%.2f%n", shape, shape.area(), shape.circ());
    }

    public static void main(String... args) {

        Shape circle = new Circle(10);
        Shape rect   = new Rectangle(5, 10);
        Shape square = new Square(3);

        printShape(circle);
        printShape(rect);
        printShape(square);

        /*
        * Exercise 5
        *
        * Show that a + a = 2 * a holds for types, where 2 is the Bool type.
        *
        * Using the Curry-Howard isomorphism:
        *
        * a v a = Bool ^ a
        *       = a                 (Bool is never Void)
        *       = a v a
        *
        * Thus, the equivalence holds.
        */
    }
}
