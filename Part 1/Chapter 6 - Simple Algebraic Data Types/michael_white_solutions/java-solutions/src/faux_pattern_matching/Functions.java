package faux_pattern_matching;

public abstract class Functions {
    private Functions() {}

    private static final float PI = 3.1415926535f;

    public static float area(final Shape s) {
        return s.match(new Shape.Visitor<Float>() {
            @Override
            public Float visitCircle(final Shape.Circle circle) {
                return PI * circle.radius() * circle.radius();
            }

            @Override
            public Float visitRect(final Shape.Rect rect) {
                return rect.width() * rect.height();
            }

            @Override
            public Float visitSquare(final Shape.Square square) {
                return square.sideLength() * square.sideLength();
            }
        });
    }

    public static float circ(final Shape s) {
        return s.match(new Shape.Visitor<Float>() {
            @Override
            public Float visitCircle(final Shape.Circle circle) {
                return 2 * PI * circle.radius();
            }

            @Override
            public Float visitRect(final Shape.Rect rect) {
                return 2 * (rect.width() + rect.height());
            }

            @Override
            public Float visitSquare(final Shape.Square square) {
                return 4 * square.sideLength();
            }
        });
    }
}
