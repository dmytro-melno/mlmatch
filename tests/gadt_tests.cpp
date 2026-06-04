
#include "../mlmatch.h"

#include <cstdio>

namespace gadt_tests {

static int failures = 0;

static void check(bool cond, const char* msg) {
    if (!cond) {
        std::printf("FAILED: %s\n", msg);
        failures += 1;
    }
}

struct term {
    int id;
};

struct value {
    int id;
};

enum class elab_mode {
    check,
    infer
};

template <elab_mode>
struct elab_result;

template <>
struct elab_result<elab_mode::check> {
    term tm;
};

template <>
struct elab_result<elab_mode::infer> {
    term tm;
    value ty;
};

template <elab_mode mode>
struct tagged_gadt(elab_t) (
    (infer, (),         (elab_t<elab_mode::infer>)),
    (check, (value ty), (elab_t<elab_mode::check>))
);

struct tagged(presyntax) (
    (var, (int id)),
    (lit, (int n))
);

template <elab_mode mode>
auto elaborate(elab_t<mode> m, presyntax ast) -> elab_result<mode> {
    return match(m, ast).with(
        [](elab_t<mode>::infer, presyntax::var var) {
            return elab_result<elab_mode::infer>{term{10 + var.id}, value{100 + var.id}};
        },
        [](elab_t<mode>::infer, presyntax::lit lit) {
            return elab_result<elab_mode::infer>{term{20 + lit.n}, value{200 + lit.n}};
        },
        [](elab_t<mode>::check chk, presyntax::var var) {
            return elab_result<elab_mode::check>{term{chk.ty.id + var.id}};
        },
        [](elab_t<mode>::check chk, presyntax::lit lit) {
            return elab_result<elab_mode::check>{term{chk.ty.id + lit.n}};
        }
    );
}

enum class axis {
    x,
    y
};

enum class polarity {
    pos,
    neg
};

template <axis axis_v, polarity polarity_v>
struct tagged_gadt(indexed) (
    (x_pos_a, (int payload), (indexed<axis::x, polarity::pos>)),
    (x_pos_b, (int payload), (indexed<axis::x, polarity::pos>)),
    (x_neg,   (int payload), (indexed<axis::x, polarity::neg>)),
    (y_pos,   (int payload), (indexed<axis::y, polarity::pos>)),
    (y_neg,   (int payload), (indexed<axis::y, polarity::neg>))
);

static_assert(elab_t<elab_mode::infer>::ctors_count == 1);
static_assert(elab_t<elab_mode::check>::ctors_count == 1);
static_assert(indexed<axis::x, polarity::pos>::ctors_count == 2);
static_assert(indexed<axis::x, polarity::neg>::ctors_count == 1);
static_assert(indexed<axis::y, polarity::pos>::ctors_count == 1);
static_assert(indexed<axis::y, polarity::neg>::ctors_count == 1);

template <axis axis_v, polarity polarity_v>
int eval_indexed(indexed<axis_v, polarity_v> x) {
    return match(x).with(
        [](indexed<axis_v, polarity_v>::x_pos_a c) { return 1000 + c.payload; },
        [](indexed<axis_v, polarity_v>::x_pos_b c) { return 2000 + c.payload; },
        [](indexed<axis_v, polarity_v>::x_neg c)   { return 3000 + c.payload; },
        [](indexed<axis_v, polarity_v>::y_pos c)   { return 4000 + c.payload; },
        [](indexed<axis_v, polarity_v>::y_neg c)   { return 5000 + c.payload; }
    );
}

template <axis axis_v, polarity polarity_v>
int eval_indexed_ptr(indexed<axis_v, polarity_v>* x) {
    return match(x).with(
        [](indexed<axis_v, polarity_v>::x_pos_a* c) { return 10000 + c->payload; },
        [](indexed<axis_v, polarity_v>::x_pos_b* c) { return 20000 + c->payload; },
        [](indexed<axis_v, polarity_v>::x_neg* c)   { return 30000 + c->payload; },
        [](indexed<axis_v, polarity_v>::y_pos* c)   { return 40000 + c->payload; },
        [](indexed<axis_v, polarity_v>::y_neg* c)   { return 50000 + c->payload; }
    );
}

template <axis axis_v, polarity polarity_v>
int bind_whole_indexed_value(indexed<axis_v, polarity_v> x) {
    return match(x).with(
        [](indexed<axis_v, polarity_v> whole) {
            return whole.ctors_count;
        }
    );
}

template <axis axis_v, polarity polarity_v>
int wildcard_indexed_value(indexed<axis_v, polarity_v> x) {
    return match(x).with(
        [](_) {
            return 77;
        }
    );
}

struct tagged(flag) (
    (on,  ()),
    (off, ())
);

template <axis axis_v, polarity polarity_v>
int match_indexed_and_flag(indexed<axis_v, polarity_v> x, flag f) {
    return match(x, f).with(
        [](indexed<axis_v, polarity_v>::x_pos_a c, flag::on)  { return 10 + c.payload; },
        [](indexed<axis_v, polarity_v>::x_pos_a c, flag::off) { return 20 + c.payload; },
        [](indexed<axis_v, polarity_v>::x_pos_b c, flag::on)  { return 30 + c.payload; },
        [](indexed<axis_v, polarity_v>::x_pos_b c, flag::off) { return 40 + c.payload; },
        [](indexed<axis_v, polarity_v>::x_neg c,   flag::on)  { return 50 + c.payload; },
        [](indexed<axis_v, polarity_v>::x_neg c,   flag::off) { return 60 + c.payload; },
        [](indexed<axis_v, polarity_v>::y_pos c,   flag::on)  { return 70 + c.payload; },
        [](indexed<axis_v, polarity_v>::y_pos c,   flag::off) { return 80 + c.payload; },
        [](indexed<axis_v, polarity_v>::y_neg c,   flag::on)  { return 90 + c.payload; },
        [](indexed<axis_v, polarity_v>::y_neg c,   flag::off) { return 100 + c.payload; }
    );
}

void run_elab_tests() {
    auto infer_mode = elab_t<elab_mode::infer>::infer::make();
    auto check_mode = elab_t<elab_mode::check>::check::make(value{30});
    auto var = presyntax::var::make(7);
    auto lit = presyntax::lit::make(5);

    auto infer_var = elaborate(infer_mode, var);
    auto infer_lit = elaborate(infer_mode, lit);
    auto check_var = elaborate(check_mode, var);
    auto check_lit = elaborate(check_mode, lit);

    check(infer_var.tm.id == 17 && infer_var.ty.id == 107, "infer/var result");
    check(infer_lit.tm.id == 25 && infer_lit.ty.id == 205, "infer/lit result");
    check(check_var.tm.id == 37, "check/var result");
    check(check_lit.tm.id == 35, "check/lit result");
}

void run_indexed_tests() {
    auto xpa = indexed<axis::x, polarity::pos>::x_pos_a::make(1);
    auto xpb = indexed<axis::x, polarity::pos>::x_pos_b::make(2);
    auto xn = indexed<axis::x, polarity::neg>::x_neg::make(3);
    auto yp = indexed<axis::y, polarity::pos>::y_pos::make(4);
    auto yn = indexed<axis::y, polarity::neg>::y_neg::make(5);

    check(eval_indexed(xpa) == 1001, "multi-index x_pos_a");
    check(eval_indexed(xpb) == 2002, "multi-index x_pos_b");
    check(eval_indexed(xn) == 3003, "multi-index x_neg");
    check(eval_indexed(yp) == 4004, "multi-index y_pos");
    check(eval_indexed(yn) == 5005, "multi-index y_neg");

    check(eval_indexed_ptr(&xpa) == 10001, "multi-index pointer x_pos_a");
    check(eval_indexed_ptr(&xpb) == 20002, "multi-index pointer x_pos_b");
    check(eval_indexed_ptr(&xn) == 30003, "multi-index pointer x_neg");
    check(eval_indexed_ptr(&yp) == 40004, "multi-index pointer y_pos");
    check(eval_indexed_ptr(&yn) == 50005, "multi-index pointer y_neg");

    check(bind_whole_indexed_value(xpa) == 2, "bind whole value with two possible constructors");
    check(bind_whole_indexed_value(xn) == 1, "bind whole value with one possible constructor");
    check(wildcard_indexed_value(yp) == 77, "wildcard GADT value");

    check(match_indexed_and_flag(xpa, flag::on::make()) == 11, "multi-scrutinee x_pos_a/on");
    check(match_indexed_and_flag(xpa, flag::off::make()) == 21, "multi-scrutinee x_pos_a/off");
    check(match_indexed_and_flag(xpb, flag::on::make()) == 32, "multi-scrutinee x_pos_b/on");
    check(match_indexed_and_flag(xn, flag::off::make()) == 63, "multi-scrutinee x_neg/off");
    check(match_indexed_and_flag(yp, flag::on::make()) == 74, "multi-scrutinee y_pos/on");
    check(match_indexed_and_flag(yn, flag::off::make()) == 105, "multi-scrutinee y_neg/off");
}

} // namespace gadt_tests

int main() {
    gadt_tests::run_elab_tests();
    gadt_tests::run_indexed_tests();

    if (gadt_tests::failures == 0) {
        std::printf("gadt_tests: ok\n");
    }

    return gadt_tests::failures == 0 ? 0 : 1;
}
