#include <cassert>
#include <cmath>
#include <optional>
#include <iostream>

// book code

std::optional<double> safe_root(double x) {
    return x >= 0 ? std::optional{std::sqrt(x)} : std::nullopt;
}

// 1. Kleisli Category for Partial Functions
// Compose: Pretty straightforward akin to the book except in this
// case we're embellishing (checking for a value) with an optional

const auto compose = [](auto f, auto g) {
    return [f, g](auto x) {
        const auto p1 = f(x);
        const auto p2 = p1.has_value() ? g(p1.value()) : std::nullopt;
        return p2;
    };
};

// Identity: the "return" morphism he defines in the book
const auto identity = [](auto x) {
    return x;
};

// 2. Safe reciprocal

auto safe_reciprocal(double x) {
    return x ? std::optional<double>{1.0 / x} : std::nullopt;
}

// 3. safe_root_reciprocal
auto safe_root_reciprocal(double x) {
    return compose(safe_reciprocal, safe_root)(x);
}

int main() {
    // T0
    assert(safe_root(-5) == std::nullopt);
    assert(safe_root(64) == 8);

    // T2
    assert(safe_reciprocal(3 * 0) == std::nullopt);
    std::cout << safe_reciprocal(-25).value() << std::endl;

    // T1,T3
    assert(safe_root_reciprocal(-2) == std::nullopt);
    assert(safe_root_reciprocal(0) == std::nullopt);
    assert(safe_root_reciprocal(0.01) == 10);
    // check identity works (so that it is in fact a category)
    assert(identity(safe_root_reciprocal)(0.01) == 10);
    assert(identity(safe_root_reciprocal(1)).value() == 1);
}