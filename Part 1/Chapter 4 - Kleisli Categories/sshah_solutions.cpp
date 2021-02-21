#include <optional>
#include <cmath>
#include <cassert>
#include <concepts>

// http://coliru.stacked-crooked.com/a/0d9f494dbd834ea2
// C++17 alternative with std::function: http://coliru.stacked-crooked.com/a/d8a8ed976dc59715

namespace detail{
template<typename>
struct unary_fn;

template<typename M, typename R, typename Arg>
struct unary_fn<R(M::*)(Arg)>
{
    using return_type = R;
    using arg_type = Arg;
};

template<typename R, typename Arg>
struct unary_fn<R(*)(Arg)>
{
    using return_type = R;
    using arg_type = Arg;
};
}

// Concept to check composability
template <typename PartialFn1, typename PartialFn2, typename OutType, typename InType = detail::unary_fn<PartialFn1>::arg_type>
concept SingleArgComposable = requires(PartialFn1 pf1, PartialFn2 pf2, InType elem) {
    { pf2(*pf1(elem)) } -> std::same_as<OutType>;
};

// Given(using std::optional) and safe_root
std::optional<double> safe_root(double x) {
    return x >= 0 ? std::optional{std::sqrt(x)} : std::nullopt;
}

template <typename T>
std::optional<T> id(T input) {
    return input;
}

// Q1: Construct the Kleisli category for partial functions (define composition and identity).
template <typename PartialFn1, typename PartialFn2, typename RetType = detail::unary_fn<PartialFn2>::return_type> requires SingleArgComposable<PartialFn1, PartialFn2, RetType>
auto compose(PartialFn1 firstCallable, PartialFn2 secondCallable) {
    return [firstCallable, secondCallable](auto inValue) {
        auto firstResult = firstCallable(inValue);
        if(firstResult) {
            return secondCallable(*firstResult);
        }
        return RetType{};
    };
}

// Q2: Implement safe_reciprocal
std::optional<double> safe_reciprocal(double input) {
    return (input != 0) ? std::optional{1 / input} : std::nullopt;
}


int main(){
    // Q1(Testing id + compose)
    assert((safe_root(4.) == compose(id<double>, safe_root)(4.)) && safe_root(4.) == std::optional<double>{2.});
    assert((safe_root(4.) == compose(safe_root, id<double>)(4.)) && safe_root(4.) == std::optional<double>{2.});
    assert((safe_root(-5.) == compose(safe_root, id<double>)(-5.)) && safe_root(-5.) == std::nullopt);

    // Q3: Compose safe_root and safe_reciprocal
    auto safe_root_reciprocal = compose(safe_reciprocal, safe_root);
    assert(*safe_root_reciprocal(0.25) == 2.0);
    assert(safe_root_reciprocal(0) == std::nullopt);
    assert(safe_root_reciprocal(-0.25) == std::nullopt);
}
