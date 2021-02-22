// Godbolt: https://godbolt.org/z/fMdqY3

// START: Copied from Conor Hoekstra
auto compose(auto f, auto g) {
    return [f, g] (auto x) {
        auto const res = f(x);
        return res.has_value() ? g(res.value()) : std::nullopt;
    };
}


auto safe_root(double n) -> std::optional<double> {
    return n >= 0 ? std::optional{sqrt(n)} : std::nullopt; 
}

auto safe_reciprocal(int n) -> std::optional<double> {
    return n != 0 ? std::optional{1.0 / n} : std::nullopt; 
}
// END: Copied from Conor Hoekstra

// Question 3
//
// Slightly modified: we do have first class functions in C++, no need
// to wrap them in lambdas.
auto safe_root_reciprocal(int n) -> std::optional<double> {
    return compose(safe_reciprocal, safe_root)(n);
}
