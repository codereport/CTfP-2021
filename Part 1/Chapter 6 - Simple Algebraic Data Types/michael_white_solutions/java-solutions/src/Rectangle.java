public class Rectangle implements Shape {
    private final float width;
    private final float height;

    public Rectangle(final float radius, final float height) {
        this.width = radius;
        this.height = height;
    }

    @Override
    public float area() {
        return width * height;
    }

    @Override
    public float circ() {
        return 2 * (width + height);
    }
}
