# mlmatch

A header-only C++ library providing algebraic data types (also known as tagged unions or discriminated unions) and ML‑style pattern matching.
It is inspired by pattern matching features of functional languages like OCaml and Haskell,

## Features
- Concise and readable tagged unions definitions and expressive pattern matching.
- Pattern matching on multiple values simultaneously.
- Wildcard patterns for “catch-all” cases.
- Exhaustiveness checking, ensuring all cases are covered.
- Unreachable clauses detection.
- Zero runtime overhead. It generates a simple `switch`-statement tree from pattern matching clauses at compile time using template meta-programming. The generated `switch` tree is guaranteed to inspect each scrutinee value at most once.
- Compact memory representation of tagged unions.
- Pattern clauses compilation to case tree is based on Maranget's algorithm(link: https://doi.org/10.1145/1411304.1411311), guaranteeing the same matching semantics as OCaml or Haskell.

## Limitations
- Pattern matching only supported for types defined via the `tagged` macro. Matching on types such as bool, int, or std::optional is not supported.
- No nested pattern matching.
- No guard clauses (`when`).
- No GADTs/indexed data types (yet?).

## Requirements
- A C++20-compliant compiler.
- The `<type_traits>` standard header.

## Usage

### 1. Include the header
#include "mlmatch.h"

### 1. Define a tagged union
struct tagged(expr) (
    (val, (int v)),
    (add, (expr* e1, expr* e2)),
    (sub, (expr* e1, expr* e2)),
    (mul, (expr* e1, expr* e2)),
    (div, (expr* e1, expr* e2)),
);

### 1. Perform pattern matching
int eval(expr* e) {
    return match(*e).with(
        [](expr::val e) { return e.v; },
        [](expr::add e) { return eval(e.e1) + eval(e.e2); },
        [](expr::sub e) { return eval(e.e1) - eval(e.e2); },
        [](expr::mul e) { return eval(e.e1) * eval(e.e2); },
        [](expr::div e) { return eval(e.e1) / eval(e.e2); }
    );
}

void test_eval() {
    auto e1 = expr::val::make(100);
    auto e2 = expr::val::make(42);
    auto e3 = expr::mul::make(&e1, &e2);
    auto e4 = expr::val::make(5);
    auto e = expr::add::make(&e3, &e4);

    int res = eval(&e);

    printf("res = %i\n", res);
}
// Output:
// res = 4205


### Multiple scrutinee and "wildcard" patterns
struct tagged(color) (
    (red,   ()),
    (green, ()),
    (blue,  ())
);

struct tagged(shape) (
    (circle,    ()),
    (rectangle, ())
);

struct tagged(size) (
    (small, ()),
    (large, ())
);

#### The base type of a tagged union can be used as a wildcard pattern to match any constructor of that type.
const char* classify1(color c, shape s, size sz) {
    return match(c, s, sz).with(
        [](color::red,   shape,            size::small) { return "Red small"; },
        [](color,        shape::circle,    size::small) { return "Small circle"; },
        [](color::green, shape::rectangle, size)        { return "Green rectangle"; },
        [](color,        shape,            size::large) { return "Something large"; },
        [](color,        shape,            size)        { return "Something else"; }
    );
}

void classify1_test() {
    printf("%s\n", classify1(color::red{}, shape::circle{}, size::small{}));      // Red small
    printf("%s\n", classify1(color::blue{}, shape::circle{}, size::small{}));     // Small circle
    printf("%s\n", classify1(color::green{}, shape::rectangle{}, size::small{})); // Green rectangle
    printf("%s\n", classify1(color::blue{}, shape::rectangle{}, size::large{}));  // Something large
    printf("%s\n", classify1(color::blue{}, shape::rectangle{}, size::small{}));  // Something else
}
// Output:
// Red small
// Small circle
// Green rectangle
// Something large
// Something else

#### Alternatively, the special `_` type can be used as a wildcard pattern to match a value of any type. However, since a wildcard argument has type `_`, it cannot be used to bind and access the matched value.
const char* classify2(color c, shape s, size sz) {
    return match(c, s, sz).with(
        [](color::red,   _,                size::small) { return "Red small"; },
        [](_,            shape::circle,    size::small) { return "Small circle"; },
        [](color::green, shape::rectangle, _)           { return "Green rectangle"; },
        [](_,            _,                size::large) { return "Something large"; },
        [](_,            _,                _)           { return "Something else"; }
    );
}

void classify2_test() {
    printf("%s\n", classify2(color::red{}, shape::circle{}, size::small{}));      // Red small
    printf("%s\n", classify2(color::blue{}, shape::circle{}, size::small{}));     // Small circle
    printf("%s\n", classify2(color::green{}, shape::rectangle{}, size::small{})); // Green rectangle
    printf("%s\n", classify2(color::blue{}, shape::rectangle{}, size::large{}));  // Something large
    printf("%s\n", classify2(color::blue{}, shape::rectangle{}, size::small{}));  // Something else
}
// Output:
// Red small
// Small circle
// Green rectangle
// Something large
// Something else

## License
This project is licensed under the MIT License—see LICENSE for details.
