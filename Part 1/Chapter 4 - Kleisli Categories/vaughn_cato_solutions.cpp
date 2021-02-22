#include <optional>
#include <cassert>
#include <cmath>

using std::optional;



namespace partial_functions
{
    auto identity(auto x) -> optional<decltype(x)>
    {
        return x;
    }

    auto compose(auto a, auto b)
    {
        return [a,b](auto x) -> decltype(b(*a(x)))
        {
            if (auto ax = a(x)) return b(*ax);
            return {};
        };
    };

    auto safe_root = [](auto x) -> optional<decltype(x)> 
    {
        if (x >= 0) return std::sqrt(x);
        return {};
    };

    auto safe_reciprocal = [](auto x) -> optional<decltype(x)>
    {
        if (x != 0) return 1/x;
        return {};
    };

    auto safe_root_reciprocal = compose(safe_root, safe_reciprocal);
}


int main()
{
    using partial_functions::safe_root_reciprocal;
    assert(safe_root_reciprocal(4.0) == 0.5);
    assert(safe_root_reciprocal(0.0) == std::nullopt);
    assert(safe_root_reciprocal(-1.0) == std::nullopt);
}
