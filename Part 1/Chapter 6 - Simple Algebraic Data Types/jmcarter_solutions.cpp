// Question 2 - 3 - 4


struct Shape {
    virtual double area() = 0
    virtual double circ() = 0  // new, Q.3
}

struct Circle : public Shape {
    Circle(double radius) : r{radius} {}
    double area() override {
        return PI*r*r;
    }
    double circ() override {   // new, Q.3
        return 2*PI*r;
    }
private:
    double r;
}

struct Rect : public Shape {
    Rect(double height, double width) : h{height}, w{width} {}
    double area() override {
        return h*w;
    }
    double circ() override {    // new Q.3
        return 2*(h+w);
    }
private:
    double h;
    double w;
}

// New Q.4//
struct Square : public Shape {
    Rect(double side) : s{side} {}
    double area() override {
        return s*s;
    }
    double circ() override {
        return 4*s;
    }
private:
    double s;
}