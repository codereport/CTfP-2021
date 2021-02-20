// Godbolt: https://www.godbolt.org/z/8v6d49

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
