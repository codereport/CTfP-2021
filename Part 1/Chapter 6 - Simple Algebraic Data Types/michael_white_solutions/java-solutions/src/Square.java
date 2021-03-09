public class Square implements Shape {
    private final float sideLength;

    public Square(float sideLength) {
        this.sideLength = sideLength;
    }

    @Override
    public float area() {
        return sideLength * sideLength;
    }

    @Override
    public float circ() {
        return 4 * sideLength;
    }
}
