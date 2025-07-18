#include <cstdio>
#include "mlmatch.h"


struct tagged(expr) (
    (val, (int v)),
    (add, (expr* e1, expr* e2)),
    (sub, (expr* e1, expr* e2)),
    (mul, (expr* e1, expr* e2)),
    (div, (expr* e1, expr* e2)),
);

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

// Match on more then one value
const char* classify1(color c, shape s, size sz) {
    return match(c, s, sz).with(
        [](color::red,   shape,            size::small) { return "Red small"; },
        [](color,        shape::circle,    size::small) { return "Small circle"; },
        [](color::green, shape::rectangle, size)        { return "Green rectangle"; },
        [](color,        shape,            size::large) { return "Something large"; },
        [](color,        shape,            size)        { return "Something else"; }
    );
}

// ... using wildcard placeholder type
const char* classify2(color c, shape s, size sz) {
    return match(c, s, sz).with(
        [](color::red,   _,                size::small) { return "Red small"; },
        [](_,            shape::circle,    size::small) { return "Small circle"; },
        [](color::green, shape::rectangle, _)           { return "Green rectangle"; },
        [](_,            _,                size::large) { return "Something large"; },
        [](_,            _,                _)           { return "Something else"; }
    );
}


void classify1_test() {
    printf("%s\n", classify1(color::red{}, shape::circle{}, size::small{}));      // Red small
    printf("%s\n", classify1(color::blue{}, shape::circle{}, size::small{}));     // Small circle
    printf("%s\n", classify1(color::green{}, shape::rectangle{}, size::small{})); // Green rectangle
    printf("%s\n", classify1(color::blue{}, shape::rectangle{}, size::large{}));  // Something large
    printf("%s\n", classify1(color::blue{}, shape::rectangle{}, size::small{}));  // Something else
}


void classify2_test() {
    printf("%s\n", classify2(color::red{}, shape::circle{}, size::small{}));      // Red small
    printf("%s\n", classify2(color::blue{}, shape::circle{}, size::small{}));     // Small circle
    printf("%s\n", classify2(color::green{}, shape::rectangle{}, size::small{})); // Green rectangle
    printf("%s\n", classify2(color::blue{}, shape::rectangle{}, size::large{}));  // Something large
    printf("%s\n", classify2(color::blue{}, shape::rectangle{}, size::small{}));  // Something else
}


int main() {
    printf("test expr eval\n");
    test_eval();
    printf("\n");
    printf("test classify1\n");
    classify1_test();
    printf("\n");
    printf("test classify2\n");
    classify2_test();
    printf("\n");
}
