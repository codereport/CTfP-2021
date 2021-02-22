// Godbolt: https://www.godbolt.org/z/45bzz1

// Question 1

auto compose(auto f, auto g) {
    return [f, g] (auto x) {
        auto const res = f(x);
        return res.has_value() ? g(res.value()) : std::nullopt;
    };
}

// From Book (modified)

auto safe_root(double n) -> std::optional<double> {
    return n >= 0 ? std::optional{sqrt(n)} : std::nullopt; 
}

// Question 2

auto safe_reciprocal(int n) -> std::optional<double> {
    return n != 0 ? std::optional{1.0 / n} : std::nullopt; 
}

// Question 3

auto safe_root_reciprocal(int n) -> std::optional<double> {
    auto const r = safe_reciprocal(n);
    return r.has_value() ? safe_root(r.value()) : std::nullopt;
}

auto safe_root_reciprocal2(int n) -> std::optional<double> {
    return compose(safe_reciprocal, safe_root)(n);
}
