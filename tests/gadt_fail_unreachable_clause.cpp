
#include "../mlmatch.h"

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
    (x_neg,   (int payload), (indexed<axis::x, polarity::neg>)),
    (x_pos_a, (int payload), (indexed<axis::x, polarity::pos>)),
    (x_pos_b, (int payload), (indexed<axis::x, polarity::pos>)),
    (y_pos,   (int payload), (indexed<axis::y, polarity::pos>)),
    (y_neg,   (int payload), (indexed<axis::y, polarity::neg>))
);


int unreachable_clause(indexed<axis::x, polarity::pos> x) {
    // Expected: mlmatch_unreachable_clause for the second clause.
    return match(x).with(
        [](indexed<axis::x, polarity::pos>) {
            return 0;
        },
        [](indexed<axis::x, polarity::pos>::x_pos_a c) {
            return c.payload;
        },
        [](indexed<axis::x, polarity::pos>::x_pos_b c) {
            return c.payload;
        }
    );
}
