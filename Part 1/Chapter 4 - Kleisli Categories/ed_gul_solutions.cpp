#include <iostream>
#include <utility>
#include <cmath>

// optional - from the text
template<class A> class optional {
    bool _isValid;
    A _value;
public:
    optional() : _isValid(false) {}
    optional(A v) : _isValid(true), _value(v) {}
    bool isValid() const { return _isValid; }
    A value() const { return _value; }
};

// safe_root - from the text 
optional <double> safe_root( double x) {
    if (x >= 0) return optional <double> {sqrt(x)};
    else return optional <double> {};
}

// Challenge 1 - Kleisli identity  // previously defined identity works here
template<class T>
T&& partial_identity(T &&x){
    return std::forward<decltype(x)>(x);
}

// Challenge 1 - Kleisli compose // compositon implementation needs to expand to support embellished types
template<class F, class G>
auto partial_compose(F &&f, G &&g)
{
    return [&f, &g](const auto &&... args){
        auto p1 = g(args...);
        if (!p1.isValid()) return optional<decltype(f(p1.value()).value())>{};
        auto p2 = f(p1.value());
        return p2;
    };
}

// Challenge 2 - safe_reciprocal
optional<double> safe_reciprocal(int x) {
    if (x == 0) return optional<double>{};
    return optional<double>(static_cast<double>(1)/x);
}

// Challenge 3 - safe_sqrt_reciprocal
int main(void)
{
    auto safe_sqrt_reciprocal = partial_compose(safe_root, safe_reciprocal);
    std::cout << "sqrt_reciprocal(5): " << safe_sqrt_reciprocal(5).value() << std::endl; 
    std::cout << "sqrt_reciprocal(5).isValid(): " << safe_sqrt_reciprocal(5).isValid() << std::endl; 
    std::cout << "sqrt_reciprocal(0).isValid(): " << safe_sqrt_reciprocal(0).isValid() << std::endl;
}


