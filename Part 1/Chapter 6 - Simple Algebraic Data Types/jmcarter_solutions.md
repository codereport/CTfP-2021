## Challenges 6.5

### 1. Show the isomorphism between Maybe a and Either () a

Here, I know next to nothing in Haskell, but I think this is what it should look like.
```haskell
maybe_to_either :: Maybe a -> Either () a
maybe_to_either = x
    case x of 
        Nothing -> Left ()
        Just a -> Right a

either_to_maybe :: Either () a -> Maybe a
either_to_maybe = x
    case x of 
        Left () -> Nothing
        Right a -> Just a
```

### 2. Shape in C++ as abstract class, with area as virtual function

```c++
struct Shape {
    virtual double area() = 0
}

struct Circle : public Shape {
    Circle(double radius) : r{radius} {}
    double area() override {
        return PI*r*r;
    }
private:
    double r;
}

struct Rect : public Shape {
    Rect(double height, double width) : h{height}, w{width} {}
    double area() override {
        return h*w;
    }
private:
    double h;
    double w;
}
```


### 3. Add function circ to shape to compute circumference. What part of the originial code did you touch

```c++
struct Shape {
    virtual double area() = 0
    virtual double circ() = 0  // new
}

struct Circle : public Shape {
    Circle(double radius) : r{radius} {}
    double area() override {
        return PI*r*r;
    }
    double circ() override {   // new
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
    double circ() override {    // new
        return 2*(h+w);
    }
private:
    double h;
    double w;
}
```


### 4. Add a new shape to hierarchy (Square)

In haskell:

```haskell
data Shape = Circle Float
            | Rect Float Float
            | Square Float  -- new

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square s) = s * s     -- new

circ :: Shape -> Float
circ (Circle r) = 2.0 * pi * r
circ (Rect d h) = 2.0 * (d + h)
circ (Square s) = 4.0 * s   -- new

```


In C++:

```c++
// New //
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
```

Note: Here we see that when polymorphism is done via class hierarchy, it is trivial to add new types to the hierarchy without touching any other code, but adding a new function is very invasive, it touches every part of the old code. Conversely, when polymorphism is done via parametrisation, adding a new function is trivial and can be done without touching anything in the old code, whereas adding a new type is invasive, it touches every part of the old code. Uncle Bob calls this the impedence mismatch (see [this wikipedia article](https://en.wikipedia.org/wiki/Object–relational_impedance_mismatch)), and relates to the difference between a class and a data structure.

### 5. Show a + a = 2 * a, in algebra for types
To show this, it is sufficient to show an isomorphism between Either a a and (a, bool).
The isomorphism is trivial, mapping a left a to (a, True) and a right a to (a, False). The inverse is trivial

```haskell
either_to_pair :: Either a a -> (a, Bool)
either_to_pair (Left a)  = (a, True)
either_to_pair (Right a) = (a, False)

pair_to_either :: (a, Bool) -> Either a a
pair_to_either (a, True) = Left a
pair_to_either (a, False)  = Right a
```

Note that the isomorphism is not unique, we could also map Left a to (a, False) and Right a to (a, True). or Left a to (False, a) and Right a to (True, a).
