public class Circle implements Shape {
    private final float radius;
    private static final float PI = 3.1415926535f;

    public Circle(float radius) {
        this.radius = radius;
    }

    @Override
    public float area() {
        return PI * (radius * radius);
    }
}
