#### Question 1

Using `fmap _ _ = Nothing` does not work for the `Maybe` type because it doens't preserve identity (for the `Just` case).

#### Question 2

`fmap` for `Reader` is:

```hs
instance Function ((->) r) where
   fmap = (.)
```
Proof for preservation of identity:
```hs
fmap id f = id .g
          = g
          = id g
```
Proof for preservation of composition:
```hs
fmap (g . h) f = (g . f) h
               = g . (f . h)
               = g . (fmap f h)
               = fmap g (fmap f h)
               = (fmap g . fmap f) h
```
#### Question 3
[Godbolt](https://www.godbolt.org/z/55EzWd)
```cpp
auto reader_fmap = [](auto f, auto g) { 
    return [&] (auto r) { return g(f(r)); }; 
};

auto string_to_float = [](auto s) { return std::stof(s); };
auto float_to_int    = [](auto f) { return static_cast<int>(f); };
auto string_to_int   = reader_fmap(string_to_float, float_to_int);

for (auto s : { "1.23", "42.42", "17.29"}) {
    fmt::print("{}\n", string_to_float(s)); // 1.23, 42.42, 17.29
    fmt::print("{}\n", string_to_int(s));   // 1     42     17
}
```
[Godbolt](https://www.godbolt.org/z/e9x5zj)

More verbose with explicit types:
```cpp
template <typename A, typename B, typename R>
struct reader {

    using AtoB = B(A);
    using RtoB = B(R);    
    using RtoA = A(R);

    auto fmap(RtoA f, AtoB g) {
        return [=] (R r) { return g(f(r)); };
    }

};

auto string_to_float = [](auto s) { return std::stof(s); };
auto float_to_int    = [](auto f) { return static_cast<int>(f); };
auto string_to_int   = 
    reader<float, int, std::string>{}.fmap(string_to_float, float_to_int);

auto string_to_int = reader_fmap(string_to_float, float_to_int);

for (auto s : { "1.23", "42.42", "17.29"}) {
    fmt::print("{}\n", string_to_float(s)); // 1.23, 42.42, 17.29
    fmt::print("{}\n", string_to_int(s));   // 1     42     17
}
```
