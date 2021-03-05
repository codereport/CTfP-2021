#include <iostream>
#include <numbers>

// http://coliru.stacked-crooked.com/a/4999244c01d9a50a

/**
 Q1: Show the isomorphism between Maybe a and Either () a.
 A: We can think of the 2 types as objects with the morphisms f, f_rev and id.
 
f :: Maybe a -> Either () a
f (Just x) = Right x
f Nothing = Left ()

f_rev :: Either () a -> Maybe a
f_rev (Left ()) = Nothing
f_rev (Right x) = Just x

main = do
    print $ (f . f_rev) (Right 10) -- id
    print $ (f_rev . f) (Just 23)  -- id
**/

/**
 * Q2-4: Implementing Shape, Rectangle, Square with the area/circ methods.
 * > A3: I had to modify the interface and add in the implementations
 * > A4: C++: I had to create a new class and add required implementations for them.
 *       Haskell: I had to add Square's data ctor to "Shape" and add implementations for the new type Square.
 *       Summary: Java/C++ require modifying the interface class when adding new functions, but not when adding a new derived type.
 *                Haskell requires no modifications when changing the interface(new fns) but requires changes to the sum type when adding a new data ctor.
**/
struct Shape{
    virtual float area() const noexcept = 0;
    virtual float circ() const noexcept = 0;
};

struct Circle: Shape{
    Circle(float radius): radius_{radius} {}
    
    float area() const noexcept override {
        return std::numbers::pi * radius_ * radius_;
    }
    
    float circ() const noexcept override {
        return 2 * std::numbers::pi * radius_;
    }
private:
    float radius_;
};

struct Rectangle: Shape{
    Rectangle(float length, float breadth): l_{length}, b_{breadth} {}
    
    float area() const noexcept override {
        return l_ * b_;
    }
    
    float circ() const noexcept override {
        return (l_ + b_)*2;
    }
private:
    float l_;
    float b_;
};

struct Square: Shape{
    Square(float length): l_{length} {}
    
    float area() const noexcept override {
        return l_ * l_;
    }
    
    float circ() const noexcept override {
        return 4*l_;
    }
private:
    float l_;
};

/**
 * Q4: Add the Square shape in Haskell:
data Shape = Circle Float
          | Rect Float Float
          | Square Float

area :: Shape -> Float
area (Square s) = s * s

circ :: Shape -> Float
circ (Square s) = 4 * s
**/

int main(){
    auto c = Circle{3};
    std::cout << "Area: " << c.area() << " Circumference: " << c.circ() << std::endl;
    
    auto r = Rectangle{3, 4};
    std::cout << "Area: " << r.area() << " Circumference: " << r.circ() << std::endl;
    
    auto s = Square{5};
    std::cout << "Area: " << s.area() << " Circumference: " << s.circ() << std::endl;
}

/**
 * Q5: Show that 𝑎 + 𝑎 = 2 × 𝑎 holds for types (up to isomorphism.)
 * => We need to show isomorphism b/w: Either a a ~= (Bool, a)
 * The below morphisms f/f_rev along with the types above and id, form said isomorphism.
f :: Either a a -> (Bool, a)
f (Left x) = (True, x)
f (Right x) = (False, x)

f_rev :: (Bool, a) -> Either a a
f_rev (True, x) = Left x
f_rev (False, x) = Right x

main = do
  print $ (f . f_rev) (True, 100) -- id
  print $ (f_rev . f) (Left 100)  -- id 
**/
