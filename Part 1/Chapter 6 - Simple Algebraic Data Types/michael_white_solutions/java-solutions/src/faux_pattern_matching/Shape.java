package faux_pattern_matching;

// This approach is based on the following post:
// https://eng.wealthfront.com/2015/02/11/pattern-matching-in-java-with-visitor/

// We are not changing equality semantics to make these value types, as this will just add noise. See this blog post
// for details on the pitfalls of object equality in Java:
// http://angelikalanger.com/Articles/JavaSolutions/SecretsOfEquals/Equals.html
public abstract class Shape {
    private Shape () {}

    public interface Visitor<T> {
        T visitCircle(Circle circle);
        T visitRect(Rect rect);
        T visitSquare(Square square);
    }

    public abstract <T> T match(Visitor <T> visitor);

    public static final class Circle extends Shape {
        private static final float PI = 3.1415926535f;
        private final float radius;

        public Circle(final float radius) {
            this.radius = radius;
        }

        public float radius() {
            return radius;
        }

        @Override
        public <T> T match(final Visitor<T> visitor) {
            return visitor.visitCircle(this);
        }
    }

    public static final class Rect extends Shape {
        private final float width;
        private final float height;

        public Rect(final float width, final float height) {
            this.width = width;
            this.height = height;
        }

        public float width() {
            return width;
        }

        public float height() {
            return height;
        }

        @Override
        public <T> T match(final Visitor<T> visitor) {
            return visitor.visitRect(this);
        }
    }

    public static final class Square extends Shape {
        private final float sideLength;

        public Square(float sideLength) {
            this.sideLength = sideLength;
        }

        public float sideLength() {
            return sideLength;
        }

        @Override
        public <T> T match(final Visitor<T> visitor) {
            return visitor.visitSquare(this);
        }
    }
}